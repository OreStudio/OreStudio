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
#ifndef ORES_NATS_DOMAIN_WIRE_FORMAT_HPP
#define ORES_NATS_DOMAIN_WIRE_FORMAT_HPP

#include "ores.nats/export.hpp"
#include <optional>
#include <string>
#include <string_view>

namespace ores::nats {

/**
 * @brief The serialization format used for every NATS message body a
 * process sends and receives, decided once at startup and never
 * renegotiated per-message.
 */
enum class wire_format {
    json,
    msgpack,
};

/**
 * @brief Parses a wire_format from its .env/CLI string spelling
 * ("json", "msgpack"), case-insensitively.
 *
 * @param name The configured value, e.g. from ORES_NATS_WIRE_FORMAT.
 * @return The parsed wire_format, or std::nullopt if @p name is not
 *         one of the recognised spellings.
 */
[[nodiscard]] ORES_NATS_EXPORT std::optional<wire_format> parse_wire_format(std::string_view name);

/**
 * @brief Renders a wire_format back to its .env/CLI string spelling.
 */
[[nodiscard]] ORES_NATS_EXPORT std::string to_string(wire_format format);

}

#endif
