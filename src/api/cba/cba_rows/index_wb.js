import {promises as fs} from "fs";
// puppeteer-extra is a drop-in replacement for puppeteer,
// it augments the installed puppeteer with plugin functionality.
// Any number of plugins can be added through `puppeteer.use()`
import puppeteer from "puppeteer-extra";
import {executablePath} from "puppeteer";
// Add stealth plugin and use defaults (all tricks to hide puppeteer usage)
import StealthPlugin from "puppeteer-extra-plugin-stealth"

puppeteer.use(StealthPlugin())

import {Cluster} from "puppeteer-cluster";

// Define settings
const NUM_WORKERS = 8
const MAX_PAGES = 64384

const getLastPage = async (subdir) => {
  let path = subdir ? `../../../../data/cba_rows/${subdir}`
      : "../../../../data/cba_rows"

  let files = await fs.readdir(path)
  return files
  .filter((fileName) => fileName.endsWith(".json"))
  .map((fileName) =>
      parseInt(fileName
      .split(".")[0]
      .split("-")[1]))
  .sort((a, b) => a - b)
  .slice(-1)[0]
}

// Main instance
// Create n workers;
// Split remaining pages in 200-page parts;
// Send each worker the function with the pages to be queried;
// When finished, clear cookies and reload;
// Repeat till finished

// const page = await browser.newPage()
// await page.setViewport({width: 800, height: 600})

// Get last written page
let lastPage = await getLastPage()
console.log(`Last found page is ${lastPage}`)

if (lastPage === MAX_PAGES) {
  console.log("All pages have already been scraped, exiting...")
  process.exit()
}

let remainingPages = MAX_PAGES - lastPage

// Create page numbers to be requested
let requests = [];
let diff = Math.ceil(remainingPages / NUM_WORKERS)
for (let i = 0; i < NUM_WORKERS; i++) {
  let request = {
    id: i,
    start: lastPage + 1 + (diff) * i,
    end: Math.min(lastPage + diff * (i + 1), MAX_PAGES)
  }
  requests.push(request)
}
// console.log(requests)
(async () => {
  const cluster = await Cluster.launch({
    concurrency: Cluster.CONCURRENCY_BROWSER,
    maxConcurrency: 4,
    puppeteerOptions: { headless: false, executablePath: executablePath()},
    workerCreationDelay: 100,
    timeout: 120000,
    puppeteer: puppeteer
  })

  requests.map((request) => cluster.execute(request, async ({ page, data: request }) => {
    // Create a new page for each request
    let lastPage = await getLastPage(request.id) || request.start - 1
    let endPage = request.end

    // Start main loop
    while (lastPage < endPage) {
      // Start navigation
      console.log(`Navigating tab ${request.id} to savelife.in.ua`)
      await page.goto('https://savelife.in.ua/reporting')

      // Wait till the Cloudflare waiting room has ended
      const incomeContainerSelector = ".income-container"
      await page.waitForSelector(incomeContainerSelector, {timeout: 0})

      // Initialize variables
      let rows = [];
      let queriedPage = lastPage + 1;

      // Main request loop
      for (; queriedPage <= endPage; queriedPage++) {
        console.log(`Tab ${request.id} is requesting page ${queriedPage}`)

        // Try to request a page, if it errors, then assume the Cloudflare timer ended, export the collected rows and refresh
        try {
          let res = await page.evaluate(async (queriedPage) => {
            return await fetch(
                `https://savelife.in.ua/wp-json/savelife/reporting/income?date_from=2022-01-01T00:00:00&date_to=2022-10-31T23:59:59&amount_from=0&amount_to=85467079&keyword=&page=${queriedPage}&per_page=20`)
            .then((res) => res.json())
          }, queriedPage)
          rows.push(res)
        } catch (e) {
          console.error(e)
          console.error(
              `Tab ${request.id} failed to get page ${queriedPage}, exporting and reloading...`)
          // Rolling back the last queried page
          queriedPage -= 1
          break
        }
      }

      // Exporting scraped rows
      if (rows.length > 0) {
        console.log(`Exporting pages ${lastPage + 1} to ${queriedPage}`)
        await fs.writeFile(
            `../../../../data/cba_rows/${request.id}/${lastPage
            + 1}-${queriedPage}.json`,
            JSON.stringify(rows))
      }

      lastPage = queriedPage
    }

    // Clean up
    console.log(
        `Scraping has ended successfully for tab ${request.id}, exiting...`)
  }))

  await cluster.idle()
  await cluster.close()
})()
