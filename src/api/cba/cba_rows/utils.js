import {promises as fs} from "fs";

import {executablePath} from "puppeteer";
import puppeteer from "puppeteer-extra";
import StealthPlugin from "puppeteer-extra-plugin-stealth";
puppeteer.use(StealthPlugin())

import {Cluster} from "puppeteer-cluster";

export const getLastPage = async (subdir) => {
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

export const getPuppeteer = () => {
  return puppeteer.launch({ headless: false, executablePath: executablePath()})
}

export const getPuppeteerCluster = (numWorkers = 1) => {
  return Cluster.launch({
    concurrency: Cluster.CONCURRENCY_BROWSER,
    maxConcurrency: numWorkers,
    puppeteerOptions: { headless: false, executablePath: executablePath()},
    workerCreationDelay: 1000,
    timeout: Number.MAX_SAFE_INTEGER,
    puppeteer: puppeteer
  })
}
