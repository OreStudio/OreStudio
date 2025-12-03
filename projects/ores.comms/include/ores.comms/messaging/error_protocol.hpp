/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#ifndef ORES_COMMS_MESSAGING_ERROR_PROTOCOL_HPP
#define ORES_COMMS_MESSAGING_ERROR_PROTOCOL_HPP

#include <string>
#include <vector>
#include <cstdint>
#include <expected>
#include "ores.comms/messaging/frame.hpp"

namespace ores::comms::messaging {

/**
 * @brief Error response message sent when request processing fails.
 */
struct error_response final {
    error_code code;
    std::string message;

    /**
     * @brief Serialize to frame payload.
     */
    static std::vector<std::byte> serialize(error_response v);

    /**
     * @brief Deserialize from frame payload.
     */
    static std::expected<error_response, error_code> deserialize(std::span<const std::byte> data);
};

/**
 * @brief Create an error response frame.
 */
frame create_error_response_frame(
    std::uint32_t sequence,
    error_code code,
    const std::string& message);

}

#endif
