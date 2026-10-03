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
#ifndef ORES_REFDATA_API_MESSAGING_NETTING_AGREEMENT_PROTOCOL_HPP
#define ORES_REFDATA_API_MESSAGING_NETTING_AGREEMENT_PROTOCOL_HPP

#include "ores.refdata.api/domain/netting_agreement.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::messaging {

struct netting_agreement_key {
    std::string agreement_number;
};

struct netting_agreement_write {
    boost::uuids::uuid id;
    std::string agreement_number;
    boost::uuids::uuid counterparty_id;
    std::string agreement_type;
    std::optional<std::string> governing_law;
    std::optional<std::string> description;
};

struct netting_agreement_change {
    netting_agreement_write write;
    ores::utility::domain::precondition precondition;
};

struct netting_agreement_removal {
    netting_agreement_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct netting_agreement_lookup {
    netting_agreement_key key;
    std::optional<ores::refdata::domain::netting_agreement> netting_agreement;
};

struct netting_agreements_filter {
    std::optional<boost::uuids::uuid> counterparty_id;
};

struct netting_agreement_event {
    boost::uuids::uuid event_id;
    netting_agreement_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct netting_agreement_version_key {
    netting_agreement_key netting_agreement;
    std::uint32_t version;
};

struct netting_agreement_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_netting_agreements_request {
    using response_type = struct list_netting_agreements_response;
    static constexpr std::string_view nats_subject = "refdata.v1.netting_agreements.list";
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
    std::optional<netting_agreements_filter> filter;
};

struct list_netting_agreements_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::netting_agreement> netting_agreements;
    std::uint64_t total;
};

struct get_netting_agreement_request {
    using response_type = struct get_netting_agreement_response;
    static constexpr std::string_view nats_subject = "refdata.v1.netting_agreements.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    netting_agreement_key key;
};

struct get_netting_agreement_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::netting_agreement> netting_agreement;
};

struct get_many_netting_agreements_request {
    using response_type = struct get_many_netting_agreements_response;
    static constexpr std::string_view nats_subject = "refdata.v1.netting_agreements.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<netting_agreement_key> keys;
};

struct get_many_netting_agreements_response {
    ores::utility::domain::result result;
    std::vector<netting_agreement_lookup> entries;
};

struct put_netting_agreement_request {
    using response_type = struct put_netting_agreement_response;
    static constexpr std::string_view nats_subject = "refdata.v1.netting_agreements.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    netting_agreement_change change;
    ores::utility::domain::change_intent intent;
};

struct put_netting_agreement_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::netting_agreement> netting_agreement;
};

struct put_many_netting_agreements_request {
    using response_type = struct put_many_netting_agreements_response;
    static constexpr std::string_view nats_subject = "refdata.v1.netting_agreements.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<netting_agreement_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_netting_agreements_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::netting_agreement> netting_agreements;
};

struct delete_netting_agreement_request {
    using response_type = struct delete_netting_agreement_response;
    static constexpr std::string_view nats_subject = "refdata.v1.netting_agreements.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    netting_agreement_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_netting_agreement_response {
    ores::utility::domain::result result;
};

struct delete_many_netting_agreements_request {
    using response_type = struct delete_many_netting_agreements_response;
    static constexpr std::string_view nats_subject = "refdata.v1.netting_agreements.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<netting_agreement_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_netting_agreements_response {
    ores::utility::domain::result result;
};

struct list_by_counterparty_id_netting_agreements_request {
    using response_type = struct list_by_counterparty_id_netting_agreements_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.netting_agreements.list_by_counterparty_id";
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
    std::optional<netting_agreements_filter> filter;
};

struct list_by_counterparty_id_netting_agreements_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::netting_agreement> netting_agreements;
    std::uint64_t total;
};

struct list_netting_agreement_versions_request {
    using response_type = struct list_netting_agreement_versions_response;
    static constexpr std::string_view nats_subject = "refdata.v1.netting_agreements_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    netting_agreement_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<netting_agreement_versions_filter> filter;
};

struct list_netting_agreement_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::netting_agreement> versions;
    std::uint64_t total;
};

struct get_netting_agreement_version_request {
    using response_type = struct get_netting_agreement_version_response;
    static constexpr std::string_view nats_subject = "refdata.v1.netting_agreements_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    netting_agreement_version_key key;
};

struct get_netting_agreement_version_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::netting_agreement> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace netting_agreement_event_subjects {
inline constexpr std::string_view created = "refdata.v1.netting_agreements_events.created";
inline constexpr std::string_view updated = "refdata.v1.netting_agreements_events.updated";
inline constexpr std::string_view deleted = "refdata.v1.netting_agreements_events.deleted";
}

}

#endif
