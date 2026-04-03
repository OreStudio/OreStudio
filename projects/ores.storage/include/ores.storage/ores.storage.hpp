/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
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
 */
#ifndef ORES_STORAGE_HPP
#define ORES_STORAGE_HPP

/**
 * @brief Generic object storage API for ORE Studio.
 *
 * Provides bucket name constants and HTTP path helpers for the generic S3-like object storage API (PUT/GET/DELETE/HEAD /api/v1/storage/{bucket}/{key}). The storage layer replaces compute-specific HTTP endpoints with a reusable abstraction that can back ORE imports, compute artifacts, and any future binary payload transfer. The filesystem-backed HTTP implementation lives in ores.http.core.
 */
namespace ores::storage {}

#endif
