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
#ifndef ORES_TRADING_API_MESSAGING_BOND_REPO_PROTOCOL_HPP
#define ORES_TRADING_API_MESSAGING_BOND_REPO_PROTOCOL_HPP

#include "ores.trading.api/domain/bond_repo.hpp"
#include "ores.trading.api/export.hpp"
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

namespace ores::trading::messaging {

struct bond_repo_key {
    boost::uuids::uuid trade_id;
};

struct bond_repo_write {
    boost::uuids::uuid trade_id;
    boost::uuids::uuid trade_activity_id;
    std::string repo_type;
    std::optional<ores::utility::decimal::decimal> repo_rate;
    std::optional<std::string> repo_index;
};

struct bond_repo_change {
    bond_repo_write write;
    ores::utility::domain::precondition precondition;
};

struct bond_repo_removal {
    bond_repo_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct bond_repo_lookup {
    bond_repo_key key;
    std::optional<ores::trading::domain::bond_repo> bond_repo;
};

struct bond_repos_filter {
    std::optional<std::vector<boost::uuids::uuid>> trade_id_one_of;
};

struct bond_repo_event {
    boost::uuids::uuid event_id;
    bond_repo_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct bond_repo_version_key {
    bond_repo_key bond_repo;
    std::uint32_t version;
};

struct bond_repo_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_bond_repos_request {
    using response_type = struct list_bond_repos_response;
    static constexpr std::string_view nats_subject = "trading.v1.bond_repos.list";
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
    std::optional<bond_repos_filter> filter;
    std::optional<std::string> as_of;
};

struct list_bond_repos_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::bond_repo> repos;
    std::uint64_t total;
};

struct get_bond_repo_request {
    using response_type = struct get_bond_repo_response;
    static constexpr std::string_view nats_subject = "trading.v1.bond_repos.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    bond_repo_key key;
};

struct get_bond_repo_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::bond_repo> bond_repo;
};

struct get_many_bond_repos_request {
    using response_type = struct get_many_bond_repos_response;
    static constexpr std::string_view nats_subject = "trading.v1.bond_repos.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<bond_repo_key> keys;
};

struct get_many_bond_repos_response {
    ores::utility::domain::result result;
    std::vector<bond_repo_lookup> entries;
};

struct put_bond_repo_request {
    using response_type = struct put_bond_repo_response;
    static constexpr std::string_view nats_subject = "trading.v1.bond_repos.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    bond_repo_change change;
    ores::utility::domain::change_intent intent;
};

struct put_bond_repo_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::bond_repo> bond_repo;
};

struct put_many_bond_repos_request {
    using response_type = struct put_many_bond_repos_response;
    static constexpr std::string_view nats_subject = "trading.v1.bond_repos.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<bond_repo_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_bond_repos_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::bond_repo> repos;
};

struct delete_bond_repo_request {
    using response_type = struct delete_bond_repo_response;
    static constexpr std::string_view nats_subject = "trading.v1.bond_repos.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    bond_repo_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_bond_repo_response {
    ores::utility::domain::result result;
};

struct delete_many_bond_repos_request {
    using response_type = struct delete_many_bond_repos_response;
    static constexpr std::string_view nats_subject = "trading.v1.bond_repos.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<bond_repo_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_bond_repos_response {
    ores::utility::domain::result result;
};

struct list_bond_repo_versions_request {
    using response_type = struct list_bond_repo_versions_response;
    static constexpr std::string_view nats_subject = "trading.v1.bond_repos_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    bond_repo_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<bond_repo_versions_filter> filter;
};

struct list_bond_repo_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::bond_repo> versions;
    std::uint64_t total;
};

struct get_bond_repo_version_request {
    using response_type = struct get_bond_repo_version_response;
    static constexpr std::string_view nats_subject = "trading.v1.bond_repos_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    bond_repo_version_key key;
};

struct get_bond_repo_version_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::bond_repo> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace bond_repo_event_subjects {
inline constexpr std::string_view created = "trading.v1.bond_repos_events.created";
inline constexpr std::string_view updated = "trading.v1.bond_repos_events.updated";
inline constexpr std::string_view deleted = "trading.v1.bond_repos_events.deleted";
}
/**
 * @brief The members of bond_repo_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const bond_repo_key&) {
    return std::tuple{
        utility::codec::field{"trade_id", &bond_repo_key::trade_id},
    };
}

/**
 * @brief The members of bond_repo_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const bond_repo_write&) {
    return std::tuple{
        utility::codec::field{"trade_id", &bond_repo_write::trade_id},
        utility::codec::field{"trade_activity_id", &bond_repo_write::trade_activity_id},
        utility::codec::field{"repo_type", &bond_repo_write::repo_type},
        utility::codec::field{"repo_rate", &bond_repo_write::repo_rate},
        utility::codec::field{"repo_index", &bond_repo_write::repo_index},
    };
}

/**
 * @brief The members of bond_repo_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const bond_repo_change&) {
    return std::tuple{
        utility::codec::field{"write", &bond_repo_change::write},
        utility::codec::field{"precondition", &bond_repo_change::precondition},
    };
}

/**
 * @brief The members of bond_repo_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const bond_repo_removal&) {
    return std::tuple{
        utility::codec::field{"key", &bond_repo_removal::key},
        utility::codec::field{"precondition", &bond_repo_removal::precondition},
    };
}

/**
 * @brief The members of bond_repo_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const bond_repo_lookup&) {
    return std::tuple{
        utility::codec::field{"key", &bond_repo_lookup::key},
        utility::codec::field{"bond_repo", &bond_repo_lookup::bond_repo},
    };
}

/**
 * @brief The members of bond_repos_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const bond_repos_filter&) {
    return std::tuple{
        utility::codec::field{"trade_id_one_of", &bond_repos_filter::trade_id_one_of},
    };
}

/**
 * @brief The members of bond_repo_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const bond_repo_event&) {
    return std::tuple{
        utility::codec::field{"event_id", &bond_repo_event::event_id},
        utility::codec::field{"key", &bond_repo_event::key},
        utility::codec::field{"action", &bond_repo_event::action},
        utility::codec::field{"version", &bond_repo_event::version},
        utility::codec::field{"occurred_at", &bond_repo_event::occurred_at},
        utility::codec::field{"correlation_id", &bond_repo_event::correlation_id},
    };
}

/**
 * @brief The members of bond_repo_version_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const bond_repo_version_key&) {
    return std::tuple{
        utility::codec::field{"bond_repo", &bond_repo_version_key::bond_repo},
        utility::codec::field{"version", &bond_repo_version_key::version},
    };
}

/**
 * @brief The members of bond_repo_versions_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const bond_repo_versions_filter&) {
    return std::tuple{
        utility::codec::field{"version", &bond_repo_versions_filter::version},
        utility::codec::field{"from_version", &bond_repo_versions_filter::from_version},
        utility::codec::field{"to_version", &bond_repo_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_bond_repos_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_bond_repos_request&) {
    return std::tuple{
        utility::codec::field{"offset", &list_bond_repos_request::offset},
        utility::codec::field{"limit", &list_bond_repos_request::limit},
        utility::codec::field{"order", &list_bond_repos_request::order},
        utility::codec::field{"filter", &list_bond_repos_request::filter},
        utility::codec::field{"as_of", &list_bond_repos_request::as_of},
    };
}

/**
 * @brief The members of list_bond_repos_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_bond_repos_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_bond_repos_response::result},
        utility::codec::field{"repos", &list_bond_repos_response::repos},
        utility::codec::field{"total", &list_bond_repos_response::total},
    };
}

/**
 * @brief The members of get_bond_repo_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_bond_repo_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_bond_repo_request::key},
    };
}

/**
 * @brief The members of get_bond_repo_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_bond_repo_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_bond_repo_response::result},
        utility::codec::field{"bond_repo", &get_bond_repo_response::bond_repo},
    };
}

/**
 * @brief The members of get_many_bond_repos_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_bond_repos_request&) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_bond_repos_request::keys},
    };
}

/**
 * @brief The members of get_many_bond_repos_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_bond_repos_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_many_bond_repos_response::result},
        utility::codec::field{"entries", &get_many_bond_repos_response::entries},
    };
}

/**
 * @brief The members of put_bond_repo_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_bond_repo_request&) {
    return std::tuple{
        utility::codec::field{"change", &put_bond_repo_request::change},
        utility::codec::field{"intent", &put_bond_repo_request::intent},
    };
}

/**
 * @brief The members of put_bond_repo_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_bond_repo_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_bond_repo_response::result},
        utility::codec::field{"bond_repo", &put_bond_repo_response::bond_repo},
    };
}

/**
 * @brief The members of put_many_bond_repos_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_bond_repos_request&) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_bond_repos_request::changes},
        utility::codec::field{"intent", &put_many_bond_repos_request::intent},
    };
}

/**
 * @brief The members of put_many_bond_repos_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_bond_repos_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_many_bond_repos_response::result},
        utility::codec::field{"repos", &put_many_bond_repos_response::repos},
    };
}

/**
 * @brief The members of delete_bond_repo_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_bond_repo_request&) {
    return std::tuple{
        utility::codec::field{"removal", &delete_bond_repo_request::removal},
        utility::codec::field{"intent", &delete_bond_repo_request::intent},
    };
}

/**
 * @brief The members of delete_bond_repo_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_bond_repo_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_bond_repo_response::result},
    };
}

/**
 * @brief The members of delete_many_bond_repos_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_bond_repos_request&) {
    return std::tuple{
        utility::codec::field{"removals", &delete_many_bond_repos_request::removals},
        utility::codec::field{"intent", &delete_many_bond_repos_request::intent},
    };
}

/**
 * @brief The members of delete_many_bond_repos_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_bond_repos_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_bond_repos_response::result},
    };
}

/**
 * @brief The members of list_bond_repo_versions_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_bond_repo_versions_request&) {
    return std::tuple{
        utility::codec::field{"key", &list_bond_repo_versions_request::key},
        utility::codec::field{"offset", &list_bond_repo_versions_request::offset},
        utility::codec::field{"limit", &list_bond_repo_versions_request::limit},
        utility::codec::field{"order", &list_bond_repo_versions_request::order},
        utility::codec::field{"filter", &list_bond_repo_versions_request::filter},
    };
}

/**
 * @brief The members of list_bond_repo_versions_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_bond_repo_versions_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_bond_repo_versions_response::result},
        utility::codec::field{"versions", &list_bond_repo_versions_response::versions},
        utility::codec::field{"total", &list_bond_repo_versions_response::total},
    };
}

/**
 * @brief The members of get_bond_repo_version_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_bond_repo_version_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_bond_repo_version_request::key},
    };
}

/**
 * @brief The members of get_bond_repo_version_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_bond_repo_version_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_bond_repo_version_response::result},
        utility::codec::field{"version", &get_bond_repo_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::bond_repo_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::bond_repo_key& v);
    static ores::trading::messaging::bond_repo_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::bond_repo_key& v);
    static ores::trading::messaging::bond_repo_key read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::bond_repo_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::bond_repo_write& v);
    static ores::trading::messaging::bond_repo_write read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::bond_repo_write& v);
    static ores::trading::messaging::bond_repo_write read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::bond_repo_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::bond_repo_change& v);
    static ores::trading::messaging::bond_repo_change read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::bond_repo_change& v);
    static ores::trading::messaging::bond_repo_change read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::bond_repo_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::bond_repo_removal& v);
    static ores::trading::messaging::bond_repo_removal read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::bond_repo_removal& v);
    static ores::trading::messaging::bond_repo_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::bond_repo_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::bond_repo_lookup& v);
    static ores::trading::messaging::bond_repo_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::bond_repo_lookup& v);
    static ores::trading::messaging::bond_repo_lookup read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::bond_repos_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::bond_repos_filter& v);
    static ores::trading::messaging::bond_repos_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::bond_repos_filter& v);
    static ores::trading::messaging::bond_repos_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::bond_repo_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::bond_repo_event& v);
    static ores::trading::messaging::bond_repo_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::bond_repo_event& v);
    static ores::trading::messaging::bond_repo_event read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::bond_repo_version_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::bond_repo_version_key& v);
    static ores::trading::messaging::bond_repo_version_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::bond_repo_version_key& v);
    static ores::trading::messaging::bond_repo_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::bond_repo_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::bond_repo_versions_filter& v);
    static ores::trading::messaging::bond_repo_versions_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::bond_repo_versions_filter& v);
    static ores::trading::messaging::bond_repo_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::list_bond_repos_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::list_bond_repos_request& v);
    static ores::trading::messaging::list_bond_repos_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::list_bond_repos_request& v);
    static ores::trading::messaging::list_bond_repos_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::list_bond_repos_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::list_bond_repos_response& v);
    static ores::trading::messaging::list_bond_repos_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::list_bond_repos_response& v);
    static ores::trading::messaging::list_bond_repos_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::get_bond_repo_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::get_bond_repo_request& v);
    static ores::trading::messaging::get_bond_repo_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::get_bond_repo_request& v);
    static ores::trading::messaging::get_bond_repo_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::get_bond_repo_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::get_bond_repo_response& v);
    static ores::trading::messaging::get_bond_repo_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::get_bond_repo_response& v);
    static ores::trading::messaging::get_bond_repo_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::get_many_bond_repos_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::get_many_bond_repos_request& v);
    static ores::trading::messaging::get_many_bond_repos_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_many_bond_repos_request& v);
    static ores::trading::messaging::get_many_bond_repos_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::get_many_bond_repos_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::get_many_bond_repos_response& v);
    static ores::trading::messaging::get_many_bond_repos_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_many_bond_repos_response& v);
    static ores::trading::messaging::get_many_bond_repos_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::put_bond_repo_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::put_bond_repo_request& v);
    static ores::trading::messaging::put_bond_repo_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::put_bond_repo_request& v);
    static ores::trading::messaging::put_bond_repo_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::put_bond_repo_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::put_bond_repo_response& v);
    static ores::trading::messaging::put_bond_repo_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::put_bond_repo_response& v);
    static ores::trading::messaging::put_bond_repo_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::put_many_bond_repos_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::put_many_bond_repos_request& v);
    static ores::trading::messaging::put_many_bond_repos_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::put_many_bond_repos_request& v);
    static ores::trading::messaging::put_many_bond_repos_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::put_many_bond_repos_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::put_many_bond_repos_response& v);
    static ores::trading::messaging::put_many_bond_repos_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::put_many_bond_repos_response& v);
    static ores::trading::messaging::put_many_bond_repos_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::delete_bond_repo_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::delete_bond_repo_request& v);
    static ores::trading::messaging::delete_bond_repo_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::delete_bond_repo_request& v);
    static ores::trading::messaging::delete_bond_repo_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::delete_bond_repo_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::delete_bond_repo_response& v);
    static ores::trading::messaging::delete_bond_repo_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::delete_bond_repo_response& v);
    static ores::trading::messaging::delete_bond_repo_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::delete_many_bond_repos_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::delete_many_bond_repos_request& v);
    static ores::trading::messaging::delete_many_bond_repos_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::delete_many_bond_repos_request& v);
    static ores::trading::messaging::delete_many_bond_repos_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::delete_many_bond_repos_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::delete_many_bond_repos_response& v);
    static ores::trading::messaging::delete_many_bond_repos_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::delete_many_bond_repos_response& v);
    static ores::trading::messaging::delete_many_bond_repos_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::list_bond_repo_versions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::list_bond_repo_versions_request& v);
    static ores::trading::messaging::list_bond_repo_versions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::list_bond_repo_versions_request& v);
    static ores::trading::messaging::list_bond_repo_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::list_bond_repo_versions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::list_bond_repo_versions_response& v);
    static ores::trading::messaging::list_bond_repo_versions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::list_bond_repo_versions_response& v);
    static ores::trading::messaging::list_bond_repo_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_bond_repo_version_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::get_bond_repo_version_request& v);
    static ores::trading::messaging::get_bond_repo_version_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_bond_repo_version_request& v);
    static ores::trading::messaging::get_bond_repo_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_bond_repo_version_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_bond_repo_version_response& v);
    static ores::trading::messaging::get_bond_repo_version_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_bond_repo_version_response& v);
    static ores::trading::messaging::get_bond_repo_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
