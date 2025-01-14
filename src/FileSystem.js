// FileSystem.js
const BASE_PATH = '/home/bismuth/plutus/workspace/scdWs/budview';

// Helper to ensure file exists
const ensureFileExists = async (filepath) => {
  try {
    await window.fs.access(filepath);
  } catch (e) {
    // File doesn't exist, create it with empty array
    await window.fs.writeFile(filepath, JSON.stringify([]));
    console.log(`Created new file at ${filepath}`);
  }
};

export const readLocalFileImpl = (filename) => () => {
  const fullPath = `${BASE_PATH}/${filename}`;
  console.log(`Attempting to read file: ${fullPath}`);
  
  return ensureFileExists(fullPath)
    .then(() => window.fs.readFile(fullPath))
    .catch(err => {
      console.error(`Error reading file ${fullPath}:`, err);
      throw err;
    });
};

export const writeLocalFileImpl = (filename) => (content) => () => {
  const fullPath = `${BASE_PATH}/${filename}`;
  console.log(`Attempting to write to file: ${fullPath}`);
  console.log('Content to write:', content);
  
  return window.fs.writeFile(fullPath, content)
    .catch(err => {
      console.error(`Error writing to file ${fullPath}:`, err);
      throw err;
    });
};