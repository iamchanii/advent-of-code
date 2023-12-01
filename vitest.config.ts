import { defineConfig } from "vitest/config";
import rescript from "@jihchi/vite-plugin-rescript";

export default defineConfig({
  plugins: [rescript()],
  test: {
    includeSource: ["**/*.bs.js"],
  },
});
