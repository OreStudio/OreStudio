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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_protocol.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_ORE_API_MESSAGING_PRICING_ENGINES_PROTOCOL_HPP
#define ORES_ORE_API_MESSAGING_PRICING_ENGINES_PROTOCOL_HPP

#include "ores.ore.api/messaging/ore_document_protocol.hpp"
#include <cstdint>
#include <string>
#include <string_view>
#include <vector>

namespace ores::ore::messaging {

/**
 * @brief One reason the export is refused, named so a client can act on it.
 *
 * A refusal reports every reason it found, not the first, so a person fixes
 * them in one pass.
 */
struct pricing_engines_export_problem {
    /**
     * @brief Which rule the export could not follow: document_unreadable,
     * file_name_missing or write_failed.
     */
    std::string code;
    std::string message;
};

/**
 * @brief Writes one pricing engine configuration as an ORE pricing engines
 * document.
 *
 * The configuration is named by the reporting configuration it fills, which is
 * what analytics' own read finds a document by.
 */
struct export_pricing_engines_request {
    using response_type = struct export_pricing_engines_response;
    static constexpr std::string_view nats_subject = "ore.v1.ops.export_pricing_engines";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string configuration_id;
};

/**
 * @brief The document, one file, and how many products it holds.
 */
struct export_pricing_engines_response {
    bool success = false;
    std::string message;
    std::uint32_t count = 0;
    std::vector<ores::ore::messaging::ore_document> files;
    std::vector<pricing_engines_export_problem> problems;
};

}

#endif
