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
#ifndef ORES_COMMS_MESSAGING_SAVE_RESULT_HPP
#define ORES_COMMS_MESSAGING_SAVE_RESULT_HPP

#include <string>
#include <vector>
#include <cstddef>

namespace ores::comms::messaging {

/**
 * @brief Result for a single entity save operation within a batch request.
 */
struct save_result final {
    bool success{false};
    std::string message;
};

/**
 * @brief Maximum number of entities allowed in a single save batch request.
 *
 * Callers with more entities must split into multiple requests, each at most
 * this many entities. The server always processes a single request atomically.
 */
constexpr std::size_t max_save_batch_size = 1000;

/**
 * @brief Build a vector of N identical save_results, all success.
 */
inline std::vector<save_result>
make_save_results_ok(std::size_t count,
    const std::string& msg = "Saved successfully") {
    return std::vector<save_result>(count, save_result{.success = true, .message = msg});
}

/**
 * @brief Build a vector of N identical save_results, all failed.
 */
inline std::vector<save_result>
make_save_results_error(std::size_t count, const std::string& msg) {
    return std::vector<save_result>(count, save_result{.success = false, .message = msg});
}

}

#endif
