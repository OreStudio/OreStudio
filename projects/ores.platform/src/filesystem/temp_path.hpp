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
#ifndef ORES_PLATFORM_FILESYSTEM_TEMP_PATH_HPP
#define ORES_PLATFORM_FILESYSTEM_TEMP_PATH_HPP

#include <filesystem>
#include <string_view>

namespace ores::platform::filesystem {

/**
 * @brief Returns a path under the system temporary directory that does
 * not exist yet.
 *
 * The name carries a random suffix, so the path is a candidate rather
 * than a reservation and two callers can be handed the same one. The
 * caller checks existence and retries, and creating a directory is
 * exclusive, so a directory claim is arbitrated by the create call. A
 * file claim is not, because opening a file that already exists
 * succeeds, so that case rests on the random suffix alone.
 */
std::filesystem::path nonexistent_temp_path(std::string_view prefix);

}

#endif
