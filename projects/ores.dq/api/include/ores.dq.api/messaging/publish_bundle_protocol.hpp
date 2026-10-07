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
#ifndef ORES_DQ_API_MESSAGING_PUBLISH_BUNDLE_PROTOCOL_HPP
#define ORES_DQ_API_MESSAGING_PUBLISH_BUNDLE_PROTOCOL_HPP

#include <optional>
#include <string>
#include <string_view>
#include <vector>

namespace ores::dq::messaging {

/**
 * @brief Asks for a bundle to be published.
 */
struct publish_bundle_request {
    using response_type = struct publish_bundle_response;
    static constexpr std::string_view nats_subject = "dq.v1.ops.publish_bundle";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    /**
     * @brief The bundle to publish.
     */
    std::string bundle_code;
    /**
     * @brief How records are written to the target tables.
     */
    std::string mode = "upsert";
    /**
     * @brief Who asked for the publication.
     */
    std::string published_by;
    /**
     * @brief Whether the whole bundle must succeed or none of it.
     */
    bool atomic = false;
    /**
     * @brief Per-dataset parameters, as JSON.
     */
    std::string params_json;
};

/**
 * @brief Reports what the publication dispatched.
 */
struct publish_bundle_response {
    /**
     * @brief Whether the publication was accepted.
     */
    bool success = false;
    /**
     * @brief Why it was refused, when it was.
     */
    std::string error_message;
    /**
     * @brief The workflow instance the publication started.
     */
    std::string instance_id;
    /**
     * @brief How many datasets were dispatched.
     */
    int datasets_dispatched = 0;
};

}

#endif
