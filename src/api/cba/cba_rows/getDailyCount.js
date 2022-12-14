import { promises as fs } from "fs";
import bluebird from "bluebird";

import {getPuppeteer} from "./utils.js";

const FIRST_DATE = "2022-01-01";
const LAST_DATE = "2022-10-31";

// Generates array of dates as strings ("YYYY-mm-dd") to be queried
const getDateSeq = (start, end) => {
  let dates = [];

  for (let i = new Date(start); i <= new Date(end); i.setDate(i.getDate() + 1)) {
    dates.push(new Date(i));
  }

  return dates.map((date) => date.toISOString().split("T")[0]);
}

let dates = getDateSeq(FIRST_DATE, LAST_DATE);

(async () => {
  const browser = await getPuppeteer()
  const page = await browser.newPage()

  console.log(`Navigating to savelife.in.ua`)
  await page.goto('https://savelife.in.ua/reporting')

  // Wait till the Cloudflare waiting room has ended
  const incomeContainerSelector = ".income-container"
  await page.waitForSelector(incomeContainerSelector, { timeout: 0 })

  await bluebird.map(dates, async (dateString) => {
    console.log(`Requesting date ${dateString}`)

    // Request a date
    let res = await page.evaluate(async (dateString) => {
      return await fetch(`https://savelife.in.ua/wp-json/savelife/reporting/income/filters?date_from=${dateString}T00:00:00&date_to=${dateString}T23:59:59&amount_from=0&amount_to=85467079&keyword=`)
      .then((res) => res.json())
    }, dateString)

    let dateCount = {
      date: dateString,
      count: res.totals.count
    }

    // Output results
    console.log("Writing dates & counts to disk...")
    let file = await fs.readFile("./cba_counts.json", "utf-8")
    let dateCounts = JSON.parse(file)
    dateCounts = dateCounts.sort((a, b) => (new Date(a.date).getTime() - (new Date(b.date).getTime())))
    dateCounts.push(dateCount)
    await fs.writeFile("./cba_counts.json", JSON.stringify(dateCounts))
  }, { concurrency: 3 })

  console.log("Exiting...")
  await browser.close()
})()
