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
#ifndef ORES_COMPUTE_WRAPPER_APP_JOB_SCRATCH_HPP
#define ORES_COMPUTE_WRAPPER_APP_JOB_SCRATCH_HPP

#include "ores.compute.wrapper/export.hpp"
#include <cstdint>
#include <filesystem>
#include <optional>

namespace ores::compute::wrapper::app {

/**
 * @brief Removes a job's scratch folder, and states what it held.
 *
 * A job's folder is scratch: its output is in the result's own location and its
 * log is in the log store by the time the job ends, so the copy on the node buys
 * nothing and costs about 3.4 MB per completed job for ever.
 *
 * A folder that is already gone is not a failure: the caller asked for the end
 * state, and it holds. A folder that cannot be removed returns nothing, so the
 * caller can say so rather than report a release that did not happen.
 *
 * @return The bytes freed, which is zero for a folder that was already gone, or
 *         nothing when the folder could not be removed.
 */
[[nodiscard]] ORES_COMPUTE_WRAPPER_EXPORT std::optional<std::uintmax_t>
release_job_folder(const std::filesystem::path& job_folder);

}

#endif
