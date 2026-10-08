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
#ifndef ORES_VARIABILITY_API_MESSAGING_OPERATIONS_PROTOCOL_HPP
#define ORES_VARIABILITY_API_MESSAGING_OPERATIONS_PROTOCOL_HPP

#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>

namespace ores::variability::messaging {

struct clear_bootstrap_mode_request {
    using response_type = struct clear_bootstrap_mode_response;
    static constexpr std::string_view nats_subject = "variability.v1.ops.clear_bootstrap_mode";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
};

struct clear_bootstrap_mode_response {
    ores::utility::domain::result result;
};

struct complete_system_onboarding_request {
    using response_type = struct complete_system_onboarding_response;
    static constexpr std::string_view nats_subject =
        "variability.v1.ops.complete_system_onboarding";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
};

struct complete_system_onboarding_response {
    ores::utility::domain::result result;
};

struct complete_party_onboarding_request {
    using response_type = struct complete_party_onboarding_response;
    static constexpr std::string_view nats_subject = "variability.v1.ops.complete_party_onboarding";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    /*
     * The party being onboarded, which is not the caller's own party.
     */
    boost::uuids::uuid party_id;
};

struct complete_party_onboarding_response {
    ores::utility::domain::result result;
};

struct get_setting_request {
    using response_type = struct get_setting_response;
    static constexpr std::string_view nats_subject = "variability.v1.ops.get_setting";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    /**
     * @brief The setting's name, which is its natural key.
     *
     * A setting is addressed by its name everywhere it is spoken about --
     * =iam.token.access_lifetime_seconds= is the setting, and nobody knows its
     * identifier -- so a caller that holds a name asks for it by name rather than
     * by looking it up in a list first.
     */
    std::string name;
};

struct get_setting_response {
    ores::utility::domain::result result;
    /**
     * @brief The setting's value as text, meaningful when the outcome is ok.
     *
     * The value is text whatever the type: =data_type= says how to read it, and a
     * caller that asked for a cron expression reads it as one.
     */
    std::string value;
    /**
     * @brief The setting's declared type, an empty string when it does not exist.
     */
    std::string data_type;
};

}

#endif
