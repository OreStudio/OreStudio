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
#ifndef ORES_MARKETDATA_API_MESSAGING_SUPPLIER_PROTOCOL_HPP
#define ORES_MARKETDATA_API_MESSAGING_SUPPLIER_PROTOCOL_HPP

#include "ores.marketdata.api/domain/supplier.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::marketdata::messaging {

struct supplier_key {
    std::string code;
};

struct supplier_write {
    std::string code;
    std::string name;
    std::string kind;
    std::string description;
    std::string contract_reference;
    std::string support;
    int display_order;
};

struct supplier_change {
    supplier_write write;
    ores::utility::domain::precondition precondition;
};

struct supplier_removal {
    supplier_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct supplier_lookup {
    supplier_key key;
    std::optional<ores::marketdata::domain::supplier> supplier;
};

struct suppliers_filter {
    std::optional<std::vector<std::string>> code_one_of;
};

struct supplier_event {
    boost::uuids::uuid event_id;
    supplier_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct supplier_version_key {
    supplier_key supplier;
    std::uint32_t version;
};

struct supplier_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_suppliers_request {
    using response_type = struct list_suppliers_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.suppliers.list";
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
    std::optional<suppliers_filter> filter;
    std::optional<std::string> as_of;
};

struct list_suppliers_response {
    ores::utility::domain::result result;
    std::vector<ores::marketdata::domain::supplier> suppliers;
    std::uint64_t total;
};

struct get_supplier_request {
    using response_type = struct get_supplier_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.suppliers.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    supplier_key key;
};

struct get_supplier_response {
    ores::utility::domain::result result;
    std::optional<ores::marketdata::domain::supplier> supplier;
};

struct get_many_suppliers_request {
    using response_type = struct get_many_suppliers_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.suppliers.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<supplier_key> keys;
};

struct get_many_suppliers_response {
    ores::utility::domain::result result;
    std::vector<supplier_lookup> entries;
};

struct put_supplier_request {
    using response_type = struct put_supplier_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.suppliers.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    supplier_change change;
    ores::utility::domain::change_intent intent;
};

struct put_supplier_response {
    ores::utility::domain::result result;
    std::optional<ores::marketdata::domain::supplier> supplier;
};

struct put_many_suppliers_request {
    using response_type = struct put_many_suppliers_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.suppliers.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<supplier_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_suppliers_response {
    ores::utility::domain::result result;
    std::vector<ores::marketdata::domain::supplier> suppliers;
};

struct delete_supplier_request {
    using response_type = struct delete_supplier_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.suppliers.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    supplier_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_supplier_response {
    ores::utility::domain::result result;
};

struct delete_many_suppliers_request {
    using response_type = struct delete_many_suppliers_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.suppliers.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<supplier_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_suppliers_response {
    ores::utility::domain::result result;
};

struct list_supplier_versions_request {
    using response_type = struct list_supplier_versions_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.suppliers_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    supplier_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<supplier_versions_filter> filter;
};

struct list_supplier_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::marketdata::domain::supplier> versions;
    std::uint64_t total;
};

struct get_supplier_version_request {
    using response_type = struct get_supplier_version_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.suppliers_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    supplier_version_key key;
};

struct get_supplier_version_response {
    ores::utility::domain::result result;
    std::optional<ores::marketdata::domain::supplier> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace supplier_event_subjects {
inline constexpr std::string_view created = "marketdata.v1.suppliers_events.created";
inline constexpr std::string_view updated = "marketdata.v1.suppliers_events.updated";
inline constexpr std::string_view deleted = "marketdata.v1.suppliers_events.deleted";
}

}

#endif
