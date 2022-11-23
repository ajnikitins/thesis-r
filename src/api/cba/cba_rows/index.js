import { promises as fs } from "fs";
// puppeteer-extra is a drop-in replacement for puppeteer,
// it augments the installed puppeteer with plugin functionality.
// Any number of plugins can be added through `puppeteer.use()`
import puppeteer from "puppeteer-extra";
import { executablePath } from "puppeteer";

// Add stealth plugin and use defaults (all tricks to hide puppeteer usage)
import StealthPlugin from "puppeteer-extra-plugin-stealth"
puppeteer.use(StealthPlugin())

const getLastRow = async () => {
  let files = await fs.readdir("../../../../data/cba_rows")
  return files.map((fileName) =>
      parseInt(fileName
      .split(".")[0]
      .split("-")[1]))
  .sort((a, b) => a - b)
  .slice(-1)[0]
}

puppeteer.launch({ headless: false, executablePath: executablePath() }).then(async browser => {
  const page = await browser.newPage()
  await page.setViewport({ width: 800, height: 600 })

  // Get last written page
  let lastPage = await getLastRow()
  console.log(`Last found page is ${lastPage}`)

  // Start main loop
  while (lastPage < 64384) {
    // Start navigation
    console.log(`Navigating to savelife.in.ua`)
    await page.goto('https://savelife.in.ua/reporting')

    // Wait till the Cloudflare waiting room has ended
    const incomeContainerSelector = ".income-container"
    await page.waitForSelector(incomeContainerSelector, { timeout: 0 })

    // Initialize variables
    let rows = [];
    let queriedPage = lastPage + 1;

    // Main request loop
    for (; queriedPage <=  64384; queriedPage++) {
      console.log(`Requesting page ${queriedPage}`)

      // Try to request a page, if it errors, then assume the Cloudflare timer ended, export the collected rows and refresh
      try {
        let res = await page.evaluate(async (queriedPage) => {
          return await fetch(`https://savelife.in.ua/wp-json/savelife/reporting/income?date_from=2022-01-01T00:00:00&date_to=2022-10-31T23:59:59&amount_from=0&amount_to=85467079&keyword=&page=${queriedPage}&per_page=20`)
          .then((res) => res.json())
        }, queriedPage)
        rows.push(res)
      } catch (e) {
        console.error(e)
        console.error(`Failed to get page ${queriedPage}, exporting and reloading...`)
        // Rolling back the last queried page
        queriedPage -= 1
        break
      }
    }

    // Exporting scraped rows
    if (rows.length > 0) {
      console.log(`Exporting pages ${lastPage + 1} to ${queriedPage}`)
      await fs.writeFile(`../../../../data/cba_rows/${lastPage + 1}-${queriedPage}.json`,
          JSON.stringify(rows))
    }

    lastPage = queriedPage
  }

  // Clean up
  console.log("Scraping has ended successfully, exiting...")
  await browser.close()
})
