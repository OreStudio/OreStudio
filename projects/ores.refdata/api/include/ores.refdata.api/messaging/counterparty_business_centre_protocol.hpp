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
#ifndef ORES_REFDATA_API_MESSAGING_COUNTERPARTY_BUSINESS_CENTRE_PROTOCOL_HPP
#define ORES_REFDATA_API_MESSAGING_COUNTERPARTY_BUSINESS_CENTRE_PROTOCOL_HPP

#include "ores.refdata.api/domain/counterparty_business_centre.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::messaging {

struct counterparty_business_centre_key {
    boost::uuids::uuid counterparty_id;
    std::string business_centre_code;
};

struct counterparty_business_centre_write {
    boost::uuids::uuid counterparty_id;
    std::string business_centre_code;
};

struct counterparty_business_centre_change {
    counterparty_business_centre_write write;
    ores::utility::domain::precondition precondition;
};

struct counterparty_business_centre_removal {
    counterparty_business_centre_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct counterparty_business_centre_lookup {
    counterparty_business_centre_key key;
    std::optional<ores::refdata::domain::counterparty_business_centre> counterparty_business_centre;
};

struct counterparty_business_centres_filter {
    std::optional<boost::uuids::uuid> counterparty_id;
    std::optional<std::vector<boost::uuids::uuid>> counterparty_id_one_of;
};

struct list_counterparty_business_centres_request {
    using response_type = struct list_counterparty_business_centres_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.counterparty_business_centres.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<counterparty_business_centres_filter> filter;
};

struct list_counterparty_business_centres_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::counterparty_business_centre> counterparty_business_centres;
    std::uint64_t total;
};

struct get_counterparty_business_centre_request {
    using response_type = struct get_counterparty_business_centre_response;
    static constexpr std::string_view nats_subject = "refdata.v1.counterparty_business_centres.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    counterparty_business_centre_key key;
};

struct get_counterparty_business_centre_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::counterparty_business_centre> counterparty_business_centre;
};

struct get_many_counterparty_business_centres_request {
    using response_type = struct get_many_counterparty_business_centres_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.counterparty_business_centres.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<counterparty_business_centre_key> keys;
};

struct get_many_counterparty_business_centres_response {
    ores::utility::domain::result result;
    std::vector<counterparty_business_centre_lookup> entries;
};

struct put_counterparty_business_centre_request {
    using response_type = struct put_counterparty_business_centre_response;
    static constexpr std::string_view nats_subject = "refdata.v1.counterparty_business_centres.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    counterparty_business_centre_change change;
    ores::utility::domain::change_intent intent;
};

struct put_counterparty_business_centre_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::counterparty_business_centre> counterparty_business_centre;
};

struct put_many_counterparty_business_centres_request {
    using response_type = struct put_many_counterparty_business_centres_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.counterparty_business_centres.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<counterparty_business_centre_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_counterparty_business_centres_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::counterparty_business_centre> counterparty_business_centres;
};

struct delete_counterparty_business_centre_request {
    using response_type = struct delete_counterparty_business_centre_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.counterparty_business_centres.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    counterparty_business_centre_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_counterparty_business_centre_response {
    ores::utility::domain::result result;
};

struct delete_many_counterparty_business_centres_request {
    using response_type = struct delete_many_counterparty_business_centres_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.counterparty_business_centres.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<counterparty_business_centre_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_counterparty_business_centres_response {
    ores::utility::domain::result result;
};

struct list_by_counterparty_id_counterparty_business_centres_request {
    using response_type = struct list_by_counterparty_id_counterparty_business_centres_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.counterparty_business_centres.list_by_counterparty_id";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    boost::uuids::uuid counterparty_id;
    ores::utility::domain::scope scope = ores::utility::domain::scope::direct;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<counterparty_business_centres_filter> filter;
};

struct list_by_counterparty_id_counterparty_business_centres_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::counterparty_business_centre> counterparty_business_centres;
    std::uint64_t total;
};

}

#endif
