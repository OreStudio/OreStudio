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
 * @brief The public contract of object storage.
 *
 * Provides the HTTP path helpers for the generic S3-like object storage API
 * (PUT/GET/HEAD/DELETE /api/v1/storage/{bucket}/{key}), and the modelled
 * operation surface both interfaces are generated from. The storage layer is
 * application-agnostic: it holds no bucket name of its own, and bucket name
 * constants are defined by each domain library (e.g. ores.compute.api).
 *
 * The implementation lives in ores.storage.core and the service in
 * ores.storage.service; a consumer links this part and no other.
 */
namespace ores::storage::api {}

#endif
