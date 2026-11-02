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
#ifndef ORES_MARKETDATA_API_MESSAGING_PROVIDER_TERMS_LIST_OPERATIONS_PROTOCOL_HPP
#define ORES_MARKETDATA_API_MESSAGING_PROVIDER_TERMS_LIST_OPERATIONS_PROTOCOL_HPP

#include <chrono>
#include <optional>
#include <string>
#include <vector>

namespace ores::marketdata::messaging {

/**
 * @brief One provider whose terms a person accepts, at the version they read.
 */
struct provider_terms_choice {
    /**
     * @brief The provider whose terms are accepted.
     */
    std::string provider_code;
    /**
     * @brief The version of the provider the person read, or zero for the latest.
     */
    int provider_version = 0;
};

/**
 * @brief What became of one provider in a list accept.
 */
struct provider_terms_outcome {
    /**
     * @brief The provider the outcome is for.
     */
    std::string provider_code;
    /**
     * @brief Whether its acceptance was recorded.
     */
    bool success = false;
    /**
     * @brief Why it was not, or what was recorded.
     */
    std::string message;
    /**
     * @brief The account recorded as accepting.
     */
    std::string accepted_by;
    /**
     * @brief When the acceptance was recorded.
     */
    std::optional<std::chrono::system_clock::time_point> accepted_at;
};

/**
 * @brief Accepts the terms of several providers in one request.
 *
 * Each provider is accepted as the single accept would accept it, with the same
 * checks, so one refusal does not stop the others: a setup that accepts every
 * public source, or a screen that accepts all it shows, learns which were recorded.
 */
struct accept_many_provider_terms_request {
    using response_type = struct accept_many_provider_terms_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.ops.accept_many_provider_terms";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    /**
     * @brief The providers whose terms are accepted, each at the version read.
     */
    std::vector<provider_terms_choice> providers;
};

/**
 * @brief One outcome a provider, in the order they were named.
 */
struct accept_many_provider_terms_response {
    /**
     * @brief Whether every provider's acceptance was recorded.
     */
    bool success = false;
    /**
     * @brief How many were recorded, and how many were not.
     */
    std::string message;
    /**
     * @brief What became of each provider, in the order they were named.
     */
    std::vector<provider_terms_outcome> outcomes;
};

}

#endif
