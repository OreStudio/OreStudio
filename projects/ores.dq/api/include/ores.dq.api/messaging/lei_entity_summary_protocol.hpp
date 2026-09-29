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
#ifndef ORES_DQ_API_MESSAGING_LEI_ENTITY_SUMMARY_PROTOCOL_HPP
#define ORES_DQ_API_MESSAGING_LEI_ENTITY_SUMMARY_PROTOCOL_HPP

#include <string>
#include <string_view>
#include <vector>

namespace ores::dq::messaging {

/**
 * @brief One legal entity, reduced to the fields the shell lists it by.
 */
struct lei_entity_summary {
    /**
     * @brief The entity's LEI.
     */
    std::string lei;
    /**
     * @brief The entity's registered legal name.
     */
    std::string entity_legal_name;
    /**
     * @brief The entity's category, as the registry classifies it.
     */
    std::string entity_category;
    /**
     * @brief The country of the entity's legal address.
     */
    std::string country;
};

/**
 * @brief Asks for the active root legal entities, optionally of one country.
 */
struct get_lei_entities_summary_request {
    using response_type = struct get_lei_entities_summary_response;
    static constexpr std::string_view nats_subject = "dq.v1.lei-entities.summary";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    /**
     * @brief The country to list, or empty for every country's count.
     */
    std::string country_filter;
    /**
     * @brief How many rows to skip.
     */
    int offset = 0;
    /**
     * @brief How many rows to return.
     */
    int limit = 1000;
};

/**
 * @brief Reports the summary rows, or why they could not be read.
 */
struct get_lei_entities_summary_response {
    /**
     * @brief Whether the read completed.
     */
    bool success = false;
    /**
     * @brief Why it failed, when it did.
     */
    std::string error_message;
    /**
     * @brief The summarised entities.
     */
    std::vector<lei_entity_summary> entities;
};

/**
 * @brief One legal entity a search matched, and the work it would bring.
 */
struct lei_entity_match {
    /**
     * @brief The entity's LEI.
     */
    std::string lei;
    /**
     * @brief The entity's registered legal name.
     */
    std::string entity_legal_name;
    /**
     * @brief The entity's category, as the registry classifies it.
     */
    std::string entity_category;
    /**
     * @brief The country of the entity's legal address.
     */
    std::string country;
    /**
     * @brief How many parties importing this entity's hierarchy would create,
     * counting the entity itself.
     *
     * A person choosing the entity a tenant is built around is choosing that much
     * work, so the match states it before the choice is made. It is the size of the
     * hierarchy the deployment holds under the entity, which is what the
     * publication walks.
     */
    std::int64_t party_count = 0;
};

/**
 * @brief Asks for the root legal entities that match a search.
 *
 * The read a search uses, as against the country browser: a name or an LEI is
 * what somebody looking for a particular entity knows, and the deployment holds
 * far more entities than one answer can carry.
 */
struct search_lei_entities_request {
    using response_type = struct search_lei_entities_response;
    static constexpr std::string_view nats_subject = "dq.v1.lei-entities.search";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    /**
     * @brief The text to match, or empty for every entity.
     *
     * A name matches anywhere in the legal name; an LEI matches from its start.
     */
    std::string search;
    /**
     * @brief The country to restrict the matches to, or empty for every country.
     */
    std::string country_filter;
    /**
     * @brief How many matches to skip.
     */
    int offset = 0;
    /**
     * @brief How many matches to return.
     */
    int limit = 20;
};

/**
 * @brief Reports the matches, or why they could not be read.
 */
struct search_lei_entities_response {
    /**
     * @brief Whether the read completed.
     */
    bool success = false;
    /**
     * @brief Why it failed, when it did.
     */
    std::string error_message;
    /**
     * @brief The matches.
     */
    std::vector<lei_entity_match> entities;
};

}

#endif
