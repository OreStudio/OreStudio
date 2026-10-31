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
#ifndef ORES_REFDATA_API_MESSAGING_PORTFOLIO_PROTOCOL_HPP
#define ORES_REFDATA_API_MESSAGING_PORTFOLIO_PROTOCOL_HPP

#include "ores.refdata.api/domain/portfolio.hpp"
#include "ores.refdata.api/export.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include "ores.utility/codec/wire_traits_fwd.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstddef>
#include <cstdint>
#include <optional>
#include <span>
#include <string>
#include <tuple>
#include <vector>

namespace ores::refdata::messaging {

struct portfolio_key {
    std::string name;
};

struct portfolio_write {
    boost::uuids::uuid id;
    boost::uuids::uuid party_id;
    std::string name;
    std::string description;
    std::optional<boost::uuids::uuid> parent_portfolio_id;
    std::optional<boost::uuids::uuid> owner_unit_id;
    std::string purpose_type;
    std::string aggregation_ccy;
    bool is_virtual;
    std::optional<boost::uuids::uuid> sandbox_id;
    std::string status;
};

struct portfolio_change {
    portfolio_write write;
    ores::utility::domain::precondition precondition;
};

struct portfolio_removal {
    portfolio_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct portfolio_lookup {
    portfolio_key key;
    std::optional<ores::refdata::domain::portfolio> portfolio;
};

struct portfolios_filter {
    std::optional<std::optional<boost::uuids::uuid>> sandbox_id;
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
    std::optional<std::vector<boost::uuids::uuid>> sandbox_id_one_of;
};

struct portfolio_event {
    boost::uuids::uuid event_id;
    portfolio_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct portfolio_version_key {
    portfolio_key portfolio;
    std::uint32_t version;
};

struct portfolio_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_portfolios_request {
    using response_type = struct list_portfolios_response;
    static constexpr std::string_view nats_subject = "refdata.v1.portfolios.list";
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
    std::optional<portfolios_filter> filter;
    std::optional<std::string> as_of;
};

struct list_portfolios_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::portfolio> portfolios;
    std::uint64_t total;
};

struct get_portfolio_request {
    using response_type = struct get_portfolio_response;
    static constexpr std::string_view nats_subject = "refdata.v1.portfolios.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    portfolio_key key;
};

struct get_portfolio_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::portfolio> portfolio;
};

struct get_many_portfolios_request {
    using response_type = struct get_many_portfolios_response;
    static constexpr std::string_view nats_subject = "refdata.v1.portfolios.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<portfolio_key> keys;
};

struct get_many_portfolios_response {
    ores::utility::domain::result result;
    std::vector<portfolio_lookup> entries;
};

struct put_portfolio_request {
    using response_type = struct put_portfolio_response;
    static constexpr std::string_view nats_subject = "refdata.v1.portfolios.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    portfolio_change change;
    ores::utility::domain::change_intent intent;
};

struct put_portfolio_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::portfolio> portfolio;
};

struct put_many_portfolios_request {
    using response_type = struct put_many_portfolios_response;
    static constexpr std::string_view nats_subject = "refdata.v1.portfolios.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<portfolio_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_portfolios_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::portfolio> portfolios;
};

struct delete_portfolio_request {
    using response_type = struct delete_portfolio_response;
    static constexpr std::string_view nats_subject = "refdata.v1.portfolios.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    portfolio_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_portfolio_response {
    ores::utility::domain::result result;
};

struct delete_many_portfolios_request {
    using response_type = struct delete_many_portfolios_response;
    static constexpr std::string_view nats_subject = "refdata.v1.portfolios.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<portfolio_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_portfolios_response {
    ores::utility::domain::result result;
};

struct list_by_sandbox_id_portfolios_request {
    using response_type = struct list_by_sandbox_id_portfolios_response;
    static constexpr std::string_view nats_subject = "refdata.v1.portfolios.list_by_sandbox_id";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::optional<boost::uuids::uuid> sandbox_id;
    ores::utility::domain::scope scope = ores::utility::domain::scope::direct;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<portfolios_filter> filter;
};

struct list_by_sandbox_id_portfolios_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::portfolio> portfolios;
    std::uint64_t total;
};

struct list_portfolio_versions_request {
    using response_type = struct list_portfolio_versions_response;
    static constexpr std::string_view nats_subject = "refdata.v1.portfolios_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    portfolio_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<portfolio_versions_filter> filter;
};

struct list_portfolio_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::portfolio> versions;
    std::uint64_t total;
};

struct get_portfolio_version_request {
    using response_type = struct get_portfolio_version_response;
    static constexpr std::string_view nats_subject = "refdata.v1.portfolios_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    portfolio_version_key key;
};

struct get_portfolio_version_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::portfolio> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace portfolio_event_subjects {
inline constexpr std::string_view created = "refdata.v1.portfolios_events.created";
inline constexpr std::string_view updated = "refdata.v1.portfolios_events.updated";
inline constexpr std::string_view deleted = "refdata.v1.portfolios_events.deleted";
}
/**
 * @brief The members of portfolio_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const portfolio_key&) {
    return std::tuple{
        utility::codec::field{"name", &portfolio_key::name},
    };
}

/**
 * @brief The members of portfolio_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const portfolio_write&) {
    return std::tuple{
        utility::codec::field{"id", &portfolio_write::id},
        utility::codec::field{"party_id", &portfolio_write::party_id},
        utility::codec::field{"name", &portfolio_write::name},
        utility::codec::field{"description", &portfolio_write::description},
        utility::codec::field{"parent_portfolio_id", &portfolio_write::parent_portfolio_id},
        utility::codec::field{"owner_unit_id", &portfolio_write::owner_unit_id},
        utility::codec::field{"purpose_type", &portfolio_write::purpose_type},
        utility::codec::field{"aggregation_ccy", &portfolio_write::aggregation_ccy},
        utility::codec::field{"is_virtual", &portfolio_write::is_virtual},
        utility::codec::field{"sandbox_id", &portfolio_write::sandbox_id},
        utility::codec::field{"status", &portfolio_write::status},
    };
}

/**
 * @brief The members of portfolio_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const portfolio_change&) {
    return std::tuple{
        utility::codec::field{"write", &portfolio_change::write},
        utility::codec::field{"precondition", &portfolio_change::precondition},
    };
}

/**
 * @brief The members of portfolio_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const portfolio_removal&) {
    return std::tuple{
        utility::codec::field{"key", &portfolio_removal::key},
        utility::codec::field{"precondition", &portfolio_removal::precondition},
    };
}

/**
 * @brief The members of portfolio_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const portfolio_lookup&) {
    return std::tuple{
        utility::codec::field{"key", &portfolio_lookup::key},
        utility::codec::field{"portfolio", &portfolio_lookup::portfolio},
    };
}

/**
 * @brief The members of portfolios_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const portfolios_filter&) {
    return std::tuple{
        utility::codec::field{"sandbox_id", &portfolios_filter::sandbox_id},
        utility::codec::field{"id_one_of", &portfolios_filter::id_one_of},
        utility::codec::field{"sandbox_id_one_of", &portfolios_filter::sandbox_id_one_of},
    };
}

/**
 * @brief The members of portfolio_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const portfolio_event&) {
    return std::tuple{
        utility::codec::field{"event_id", &portfolio_event::event_id},
        utility::codec::field{"key", &portfolio_event::key},
        utility::codec::field{"action", &portfolio_event::action},
        utility::codec::field{"version", &portfolio_event::version},
        utility::codec::field{"occurred_at", &portfolio_event::occurred_at},
        utility::codec::field{"correlation_id", &portfolio_event::correlation_id},
    };
}

/**
 * @brief The members of portfolio_version_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const portfolio_version_key&) {
    return std::tuple{
        utility::codec::field{"portfolio", &portfolio_version_key::portfolio},
        utility::codec::field{"version", &portfolio_version_key::version},
    };
}

/**
 * @brief The members of portfolio_versions_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const portfolio_versions_filter&) {
    return std::tuple{
        utility::codec::field{"version", &portfolio_versions_filter::version},
        utility::codec::field{"from_version", &portfolio_versions_filter::from_version},
        utility::codec::field{"to_version", &portfolio_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_portfolios_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_portfolios_request&) {
    return std::tuple{
        utility::codec::field{"offset", &list_portfolios_request::offset},
        utility::codec::field{"limit", &list_portfolios_request::limit},
        utility::codec::field{"order", &list_portfolios_request::order},
        utility::codec::field{"filter", &list_portfolios_request::filter},
        utility::codec::field{"as_of", &list_portfolios_request::as_of},
    };
}

/**
 * @brief The members of list_portfolios_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_portfolios_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_portfolios_response::result},
        utility::codec::field{"portfolios", &list_portfolios_response::portfolios},
        utility::codec::field{"total", &list_portfolios_response::total},
    };
}

/**
 * @brief The members of get_portfolio_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_portfolio_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_portfolio_request::key},
    };
}

/**
 * @brief The members of get_portfolio_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_portfolio_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_portfolio_response::result},
        utility::codec::field{"portfolio", &get_portfolio_response::portfolio},
    };
}

/**
 * @brief The members of get_many_portfolios_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_portfolios_request&) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_portfolios_request::keys},
    };
}

/**
 * @brief The members of get_many_portfolios_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_portfolios_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_many_portfolios_response::result},
        utility::codec::field{"entries", &get_many_portfolios_response::entries},
    };
}

/**
 * @brief The members of put_portfolio_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_portfolio_request&) {
    return std::tuple{
        utility::codec::field{"change", &put_portfolio_request::change},
        utility::codec::field{"intent", &put_portfolio_request::intent},
    };
}

/**
 * @brief The members of put_portfolio_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_portfolio_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_portfolio_response::result},
        utility::codec::field{"portfolio", &put_portfolio_response::portfolio},
    };
}

/**
 * @brief The members of put_many_portfolios_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_portfolios_request&) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_portfolios_request::changes},
        utility::codec::field{"intent", &put_many_portfolios_request::intent},
    };
}

/**
 * @brief The members of put_many_portfolios_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_portfolios_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_many_portfolios_response::result},
        utility::codec::field{"portfolios", &put_many_portfolios_response::portfolios},
    };
}

/**
 * @brief The members of delete_portfolio_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_portfolio_request&) {
    return std::tuple{
        utility::codec::field{"removal", &delete_portfolio_request::removal},
        utility::codec::field{"intent", &delete_portfolio_request::intent},
    };
}

/**
 * @brief The members of delete_portfolio_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_portfolio_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_portfolio_response::result},
    };
}

/**
 * @brief The members of delete_many_portfolios_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_portfolios_request&) {
    return std::tuple{
        utility::codec::field{"removals", &delete_many_portfolios_request::removals},
        utility::codec::field{"intent", &delete_many_portfolios_request::intent},
    };
}

/**
 * @brief The members of delete_many_portfolios_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_portfolios_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_portfolios_response::result},
    };
}

/**
 * @brief The members of list_by_sandbox_id_portfolios_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_by_sandbox_id_portfolios_request&) {
    return std::tuple{
        utility::codec::field{"sandbox_id", &list_by_sandbox_id_portfolios_request::sandbox_id},
        utility::codec::field{"scope", &list_by_sandbox_id_portfolios_request::scope},
        utility::codec::field{"offset", &list_by_sandbox_id_portfolios_request::offset},
        utility::codec::field{"limit", &list_by_sandbox_id_portfolios_request::limit},
        utility::codec::field{"order", &list_by_sandbox_id_portfolios_request::order},
        utility::codec::field{"filter", &list_by_sandbox_id_portfolios_request::filter},
    };
}

/**
 * @brief The members of list_by_sandbox_id_portfolios_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_by_sandbox_id_portfolios_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_by_sandbox_id_portfolios_response::result},
        utility::codec::field{"portfolios", &list_by_sandbox_id_portfolios_response::portfolios},
        utility::codec::field{"total", &list_by_sandbox_id_portfolios_response::total},
    };
}

/**
 * @brief The members of list_portfolio_versions_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_portfolio_versions_request&) {
    return std::tuple{
        utility::codec::field{"key", &list_portfolio_versions_request::key},
        utility::codec::field{"offset", &list_portfolio_versions_request::offset},
        utility::codec::field{"limit", &list_portfolio_versions_request::limit},
        utility::codec::field{"order", &list_portfolio_versions_request::order},
        utility::codec::field{"filter", &list_portfolio_versions_request::filter},
    };
}

/**
 * @brief The members of list_portfolio_versions_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_portfolio_versions_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_portfolio_versions_response::result},
        utility::codec::field{"versions", &list_portfolio_versions_response::versions},
        utility::codec::field{"total", &list_portfolio_versions_response::total},
    };
}

/**
 * @brief The members of get_portfolio_version_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_portfolio_version_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_portfolio_version_request::key},
    };
}

/**
 * @brief The members of get_portfolio_version_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_portfolio_version_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_portfolio_version_response::result},
        utility::codec::field{"version", &get_portfolio_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::portfolio_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::portfolio_key& v);
    static ores::refdata::messaging::portfolio_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::portfolio_key& v);
    static ores::refdata::messaging::portfolio_key read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::portfolio_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::portfolio_write& v);
    static ores::refdata::messaging::portfolio_write read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::portfolio_write& v);
    static ores::refdata::messaging::portfolio_write read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::portfolio_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::portfolio_change& v);
    static ores::refdata::messaging::portfolio_change read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::portfolio_change& v);
    static ores::refdata::messaging::portfolio_change read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::portfolio_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::portfolio_removal& v);
    static ores::refdata::messaging::portfolio_removal read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::portfolio_removal& v);
    static ores::refdata::messaging::portfolio_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::portfolio_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::portfolio_lookup& v);
    static ores::refdata::messaging::portfolio_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::portfolio_lookup& v);
    static ores::refdata::messaging::portfolio_lookup read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::portfolios_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::portfolios_filter& v);
    static ores::refdata::messaging::portfolios_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::portfolios_filter& v);
    static ores::refdata::messaging::portfolios_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::portfolio_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::portfolio_event& v);
    static ores::refdata::messaging::portfolio_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::portfolio_event& v);
    static ores::refdata::messaging::portfolio_event read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::portfolio_version_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::portfolio_version_key& v);
    static ores::refdata::messaging::portfolio_version_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::portfolio_version_key& v);
    static ores::refdata::messaging::portfolio_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::portfolio_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::portfolio_versions_filter& v);
    static ores::refdata::messaging::portfolio_versions_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::portfolio_versions_filter& v);
    static ores::refdata::messaging::portfolio_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::list_portfolios_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::list_portfolios_request& v);
    static ores::refdata::messaging::list_portfolios_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::list_portfolios_request& v);
    static ores::refdata::messaging::list_portfolios_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::list_portfolios_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::list_portfolios_response& v);
    static ores::refdata::messaging::list_portfolios_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::list_portfolios_response& v);
    static ores::refdata::messaging::list_portfolios_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::get_portfolio_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::get_portfolio_request& v);
    static ores::refdata::messaging::get_portfolio_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::get_portfolio_request& v);
    static ores::refdata::messaging::get_portfolio_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::get_portfolio_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::get_portfolio_response& v);
    static ores::refdata::messaging::get_portfolio_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::get_portfolio_response& v);
    static ores::refdata::messaging::get_portfolio_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::get_many_portfolios_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::get_many_portfolios_request& v);
    static ores::refdata::messaging::get_many_portfolios_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_many_portfolios_request& v);
    static ores::refdata::messaging::get_many_portfolios_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::get_many_portfolios_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::get_many_portfolios_response& v);
    static ores::refdata::messaging::get_many_portfolios_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_many_portfolios_response& v);
    static ores::refdata::messaging::get_many_portfolios_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::put_portfolio_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::put_portfolio_request& v);
    static ores::refdata::messaging::put_portfolio_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::put_portfolio_request& v);
    static ores::refdata::messaging::put_portfolio_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::put_portfolio_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::put_portfolio_response& v);
    static ores::refdata::messaging::put_portfolio_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::put_portfolio_response& v);
    static ores::refdata::messaging::put_portfolio_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::put_many_portfolios_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::put_many_portfolios_request& v);
    static ores::refdata::messaging::put_many_portfolios_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_many_portfolios_request& v);
    static ores::refdata::messaging::put_many_portfolios_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::put_many_portfolios_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::put_many_portfolios_response& v);
    static ores::refdata::messaging::put_many_portfolios_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_many_portfolios_response& v);
    static ores::refdata::messaging::put_many_portfolios_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::delete_portfolio_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::delete_portfolio_request& v);
    static ores::refdata::messaging::delete_portfolio_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::delete_portfolio_request& v);
    static ores::refdata::messaging::delete_portfolio_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::delete_portfolio_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::delete_portfolio_response& v);
    static ores::refdata::messaging::delete_portfolio_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::delete_portfolio_response& v);
    static ores::refdata::messaging::delete_portfolio_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_many_portfolios_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_many_portfolios_request& v);
    static ores::refdata::messaging::delete_many_portfolios_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_many_portfolios_request& v);
    static ores::refdata::messaging::delete_many_portfolios_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_many_portfolios_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_many_portfolios_response& v);
    static ores::refdata::messaging::delete_many_portfolios_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_many_portfolios_response& v);
    static ores::refdata::messaging::delete_many_portfolios_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_by_sandbox_id_portfolios_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_by_sandbox_id_portfolios_request& v);
    static ores::refdata::messaging::list_by_sandbox_id_portfolios_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_by_sandbox_id_portfolios_request& v);
    static ores::refdata::messaging::list_by_sandbox_id_portfolios_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_by_sandbox_id_portfolios_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_by_sandbox_id_portfolios_response& v);
    static ores::refdata::messaging::list_by_sandbox_id_portfolios_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_by_sandbox_id_portfolios_response& v);
    static ores::refdata::messaging::list_by_sandbox_id_portfolios_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_portfolio_versions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_portfolio_versions_request& v);
    static ores::refdata::messaging::list_portfolio_versions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_portfolio_versions_request& v);
    static ores::refdata::messaging::list_portfolio_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_portfolio_versions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_portfolio_versions_response& v);
    static ores::refdata::messaging::list_portfolio_versions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_portfolio_versions_response& v);
    static ores::refdata::messaging::list_portfolio_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_portfolio_version_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::get_portfolio_version_request& v);
    static ores::refdata::messaging::get_portfolio_version_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_portfolio_version_request& v);
    static ores::refdata::messaging::get_portfolio_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_portfolio_version_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_portfolio_version_response& v);
    static ores::refdata::messaging::get_portfolio_version_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_portfolio_version_response& v);
    static ores::refdata::messaging::get_portfolio_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
