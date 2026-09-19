/** -*- mode: typescript-ts-mode; tab-width: 4; indent-tabs-mode: nil -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 3 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 *
 */

import { defineConfig } from 'vite';
import react from '@vitejs/plugin-react';
import tailwindcss from '@tailwindcss/vite';

/**
 * The browser bundle.
 *
 * The BFF serves the built bundle in production, so `/api` is proxied for the
 * development server only: one origin and a first-party session cookie, and no
 * CORS configuration in the build. The BFF port is read from the same variable
 * the BFF reads, so there is one number rather than two that can disagree.
 */
const BFF_PORT = process.env['ORES_WEB_PORT'] ?? '8080';
const WEB_PORT = Number(process.env['ORES_WEB_DEV_PORT'] ?? '5173');

export default defineConfig({
  plugins: [react(), tailwindcss()],
  server: {
    port: WEB_PORT,
    // Fail rather than slide to another port: a dev server that quietly moves is
    // a dev server whose URL somebody writes down and then cannot reach.
    strictPort: true,
    proxy: {
      '/api': {
        target: `http://127.0.0.1:${BFF_PORT}`,
        changeOrigin: false,
      },
    },
  },
  build: {
    outDir: 'dist',
    sourcemap: true,
  },
});
