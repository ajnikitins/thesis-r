import {promises as fs} from "fs"
import bluebird from "bluebird"

import { executablePath } from "puppeteer"
import puppeteer from "puppeteer-extra"
import StealthPlugin from "puppeteer-extra-plugin-stealth"

puppeteer.use(StealthPlugin())

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

// Settings
let MAX_PAGE = 64384 // Max page to get
let BATCH_SIZE = 4 // Number of pages in one batch (tolerance size for failed
let PAR_REQS = 4 // Number of requests executed in parallel

puppeteer.launch({headless: false, executablePath: executablePath()}).then(
    async browser => {
      const page = await browser.newPage()
      await page.setViewport({width: 800, height: 600})

      // Start main loop
      for (let lastPage = await getLastPage(); lastPage < MAX_PAGE; lastPage = await getLastPage()) {
        console.log(`Last found page is ${lastPage}`)

        // Start navigation
        console.log(`Navigating to savelife.in.ua`)
        await page.goto('https://savelife.in.ua/reporting')

        // Wait till the Cloudflare waiting room has ended
        const incomeContainerSelector = ".income-container"
        await page.waitForSelector(incomeContainerSelector, {timeout: 0})

        // Initialize batches
        let remainingPages = MAX_PAGE - lastPage
        let batches = Array(remainingPages).fill()
        // Create a sequence from lastPage + 1 to MAX_PAGE
        .map((e, index) => index + 1 + lastPage)
        // Split it into chunks of BATCH_SIZE pages
        .reduce((resultArray, item, index) => {
          const chunkIndex = Math.floor(index / BATCH_SIZE)

          if (!resultArray[chunkIndex]) {
            resultArray[chunkIndex] = []
          } // start a new chunk

          resultArray[chunkIndex].push(item)

          return resultArray
        }, [])

        let rows = []
        let lastBatch = []
        // Main request loop
        for (let batch of batches) {
          console.log(`Requesting batch ${batch}`)

          // Try to request a page, if it errors, then assume the Cloudflare timer ended, export the collected rows and refresh
          try {
            let res = await bluebird.map(batch, async (pageID) => {
              return await page.evaluate(async (page) => {
                return await fetch(
                    `https://savelife.in.ua/wp-json/savelife/reporting/income?date_from=2022-01-01T00:00:00&date_to=2022-10-31T23:59:59&amount_from=0&amount_to=85467079&keyword=&page=${page}&per_page=20`)
                .then((res) => res.json())
              }, pageID)
            }, { concurrency: PAR_REQS })
            // Push to result array
            rows = rows.concat(res)
            // Set last successful batch
            lastBatch = batch
          } catch (e) {
            console.error(e)
            console.error(`Failed to get batch ${batch}, exporting and reloading...`)
            break
          }
        }

        // Exporting scraped rows
        if (rows.length > 0) {
          console.log(`Exporting pages ${lastPage + 1} to ${lastBatch[lastBatch.length - 1]}`)
          await fs.writeFile(
              `../../../../data/cba_rows/${lastPage + 1}-${lastBatch[lastBatch.length - 1]}.json`,
              JSON.stringify(rows))
        }
      }

      // Clean up
      console.log("Scraping has ended successfully, exiting...")
      await browser.close()
    })
