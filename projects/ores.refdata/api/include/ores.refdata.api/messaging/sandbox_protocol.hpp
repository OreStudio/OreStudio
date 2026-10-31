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
#ifndef ORES_REFDATA_API_MESSAGING_SANDBOX_PROTOCOL_HPP
#define ORES_REFDATA_API_MESSAGING_SANDBOX_PROTOCOL_HPP

#include "ores.refdata.api/domain/sandbox.hpp"
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

struct sandbox_key {
    std::string name;
};

struct sandbox_write {
    boost::uuids::uuid id;
    std::string name;
    std::string purpose;
    boost::uuids::uuid anchor_portfolio_id;
    boost::uuids::uuid owner_account_id;
    std::string visibility;
    std::string status;
    std::chrono::year_month_day review_date;
    std::optional<std::string> description;
};

struct sandbox_change {
    sandbox_write write;
    ores::utility::domain::precondition precondition;
};

struct sandbox_removal {
    sandbox_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct sandbox_lookup {
    sandbox_key key;
    std::optional<ores::refdata::domain::sandbox> sandbox;
};

struct sandboxes_filter {
    std::optional<boost::uuids::uuid> anchor_portfolio_id;
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
    std::optional<std::vector<boost::uuids::uuid>> anchor_portfolio_id_one_of;
};

struct sandbox_event {
    boost::uuids::uuid event_id;
    sandbox_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct sandbox_version_key {
    sandbox_key sandbox;
    std::uint32_t version;
};

struct sandbox_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_sandboxes_request {
    using response_type = struct list_sandboxes_response;
    static constexpr std::string_view nats_subject = "refdata.v1.sandboxes.list";
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
    std::optional<sandboxes_filter> filter;
    std::optional<std::string> as_of;
};

struct list_sandboxes_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::sandbox> sandboxes;
    std::uint64_t total;
};

struct get_sandbox_request {
    using response_type = struct get_sandbox_response;
    static constexpr std::string_view nats_subject = "refdata.v1.sandboxes.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    sandbox_key key;
};

struct get_sandbox_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::sandbox> sandbox;
};

struct get_many_sandboxes_request {
    using response_type = struct get_many_sandboxes_response;
    static constexpr std::string_view nats_subject = "refdata.v1.sandboxes.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<sandbox_key> keys;
};

struct get_many_sandboxes_response {
    ores::utility::domain::result result;
    std::vector<sandbox_lookup> entries;
};

struct put_sandbox_request {
    using response_type = struct put_sandbox_response;
    static constexpr std::string_view nats_subject = "refdata.v1.sandboxes.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    sandbox_change change;
    ores::utility::domain::change_intent intent;
};

struct put_sandbox_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::sandbox> sandbox;
};

struct put_many_sandboxes_request {
    using response_type = struct put_many_sandboxes_response;
    static constexpr std::string_view nats_subject = "refdata.v1.sandboxes.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<sandbox_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_sandboxes_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::sandbox> sandboxes;
};

struct delete_sandbox_request {
    using response_type = struct delete_sandbox_response;
    static constexpr std::string_view nats_subject = "refdata.v1.sandboxes.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    sandbox_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_sandbox_response {
    ores::utility::domain::result result;
};

struct delete_many_sandboxes_request {
    using response_type = struct delete_many_sandboxes_response;
    static constexpr std::string_view nats_subject = "refdata.v1.sandboxes.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<sandbox_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_sandboxes_response {
    ores::utility::domain::result result;
};

struct list_by_anchor_portfolio_id_sandboxes_request {
    using response_type = struct list_by_anchor_portfolio_id_sandboxes_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.sandboxes.list_by_anchor_portfolio_id";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    boost::uuids::uuid anchor_portfolio_id;
    ores::utility::domain::scope scope = ores::utility::domain::scope::direct;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<sandboxes_filter> filter;
};

struct list_by_anchor_portfolio_id_sandboxes_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::sandbox> sandboxes;
    std::uint64_t total;
};

struct list_sandbox_versions_request {
    using response_type = struct list_sandbox_versions_response;
    static constexpr std::string_view nats_subject = "refdata.v1.sandboxes_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    sandbox_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<sandbox_versions_filter> filter;
};

struct list_sandbox_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::sandbox> versions;
    std::uint64_t total;
};

struct get_sandbox_version_request {
    using response_type = struct get_sandbox_version_response;
    static constexpr std::string_view nats_subject = "refdata.v1.sandboxes_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    sandbox_version_key key;
};

struct get_sandbox_version_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::sandbox> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace sandbox_event_subjects {
inline constexpr std::string_view created = "refdata.v1.sandboxes_events.created";
inline constexpr std::string_view updated = "refdata.v1.sandboxes_events.updated";
inline constexpr std::string_view deleted = "refdata.v1.sandboxes_events.deleted";
}
/**
 * @brief The members of sandbox_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const sandbox_key&) {
    return std::tuple{
        utility::codec::field{"name", &sandbox_key::name},
    };
}

/**
 * @brief The members of sandbox_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const sandbox_write&) {
    return std::tuple{
        utility::codec::field{"id", &sandbox_write::id},
        utility::codec::field{"name", &sandbox_write::name},
        utility::codec::field{"purpose", &sandbox_write::purpose},
        utility::codec::field{"anchor_portfolio_id", &sandbox_write::anchor_portfolio_id},
        utility::codec::field{"owner_account_id", &sandbox_write::owner_account_id},
        utility::codec::field{"visibility", &sandbox_write::visibility},
        utility::codec::field{"status", &sandbox_write::status},
        utility::codec::field{"review_date", &sandbox_write::review_date},
        utility::codec::field{"description", &sandbox_write::description},
    };
}

/**
 * @brief The members of sandbox_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const sandbox_change&) {
    return std::tuple{
        utility::codec::field{"write", &sandbox_change::write},
        utility::codec::field{"precondition", &sandbox_change::precondition},
    };
}

/**
 * @brief The members of sandbox_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const sandbox_removal&) {
    return std::tuple{
        utility::codec::field{"key", &sandbox_removal::key},
        utility::codec::field{"precondition", &sandbox_removal::precondition},
    };
}

/**
 * @brief The members of sandbox_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const sandbox_lookup&) {
    return std::tuple{
        utility::codec::field{"key", &sandbox_lookup::key},
        utility::codec::field{"sandbox", &sandbox_lookup::sandbox},
    };
}

/**
 * @brief The members of sandboxes_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const sandboxes_filter&) {
    return std::tuple{
        utility::codec::field{"anchor_portfolio_id", &sandboxes_filter::anchor_portfolio_id},
        utility::codec::field{"id_one_of", &sandboxes_filter::id_one_of},
        utility::codec::field{"anchor_portfolio_id_one_of",
                              &sandboxes_filter::anchor_portfolio_id_one_of},
    };
}

/**
 * @brief The members of sandbox_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const sandbox_event&) {
    return std::tuple{
        utility::codec::field{"event_id", &sandbox_event::event_id},
        utility::codec::field{"key", &sandbox_event::key},
        utility::codec::field{"action", &sandbox_event::action},
        utility::codec::field{"version", &sandbox_event::version},
        utility::codec::field{"occurred_at", &sandbox_event::occurred_at},
        utility::codec::field{"correlation_id", &sandbox_event::correlation_id},
    };
}

/**
 * @brief The members of sandbox_version_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const sandbox_version_key&) {
    return std::tuple{
        utility::codec::field{"sandbox", &sandbox_version_key::sandbox},
        utility::codec::field{"version", &sandbox_version_key::version},
    };
}

/**
 * @brief The members of sandbox_versions_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const sandbox_versions_filter&) {
    return std::tuple{
        utility::codec::field{"version", &sandbox_versions_filter::version},
        utility::codec::field{"from_version", &sandbox_versions_filter::from_version},
        utility::codec::field{"to_version", &sandbox_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_sandboxes_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_sandboxes_request&) {
    return std::tuple{
        utility::codec::field{"offset", &list_sandboxes_request::offset},
        utility::codec::field{"limit", &list_sandboxes_request::limit},
        utility::codec::field{"order", &list_sandboxes_request::order},
        utility::codec::field{"filter", &list_sandboxes_request::filter},
        utility::codec::field{"as_of", &list_sandboxes_request::as_of},
    };
}

/**
 * @brief The members of list_sandboxes_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_sandboxes_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_sandboxes_response::result},
        utility::codec::field{"sandboxes", &list_sandboxes_response::sandboxes},
        utility::codec::field{"total", &list_sandboxes_response::total},
    };
}

/**
 * @brief The members of get_sandbox_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_sandbox_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_sandbox_request::key},
    };
}

/**
 * @brief The members of get_sandbox_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_sandbox_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_sandbox_response::result},
        utility::codec::field{"sandbox", &get_sandbox_response::sandbox},
    };
}

/**
 * @brief The members of get_many_sandboxes_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_sandboxes_request&) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_sandboxes_request::keys},
    };
}

/**
 * @brief The members of get_many_sandboxes_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_sandboxes_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_many_sandboxes_response::result},
        utility::codec::field{"entries", &get_many_sandboxes_response::entries},
    };
}

/**
 * @brief The members of put_sandbox_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_sandbox_request&) {
    return std::tuple{
        utility::codec::field{"change", &put_sandbox_request::change},
        utility::codec::field{"intent", &put_sandbox_request::intent},
    };
}

/**
 * @brief The members of put_sandbox_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_sandbox_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_sandbox_response::result},
        utility::codec::field{"sandbox", &put_sandbox_response::sandbox},
    };
}

/**
 * @brief The members of put_many_sandboxes_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_sandboxes_request&) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_sandboxes_request::changes},
        utility::codec::field{"intent", &put_many_sandboxes_request::intent},
    };
}

/**
 * @brief The members of put_many_sandboxes_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_sandboxes_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_many_sandboxes_response::result},
        utility::codec::field{"sandboxes", &put_many_sandboxes_response::sandboxes},
    };
}

/**
 * @brief The members of delete_sandbox_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_sandbox_request&) {
    return std::tuple{
        utility::codec::field{"removal", &delete_sandbox_request::removal},
        utility::codec::field{"intent", &delete_sandbox_request::intent},
    };
}

/**
 * @brief The members of delete_sandbox_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_sandbox_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_sandbox_response::result},
    };
}

/**
 * @brief The members of delete_many_sandboxes_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_sandboxes_request&) {
    return std::tuple{
        utility::codec::field{"removals", &delete_many_sandboxes_request::removals},
        utility::codec::field{"intent", &delete_many_sandboxes_request::intent},
    };
}

/**
 * @brief The members of delete_many_sandboxes_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_sandboxes_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_sandboxes_response::result},
    };
}

/**
 * @brief The members of list_by_anchor_portfolio_id_sandboxes_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_by_anchor_portfolio_id_sandboxes_request&) {
    return std::tuple{
        utility::codec::field{"anchor_portfolio_id",
                              &list_by_anchor_portfolio_id_sandboxes_request::anchor_portfolio_id},
        utility::codec::field{"scope", &list_by_anchor_portfolio_id_sandboxes_request::scope},
        utility::codec::field{"offset", &list_by_anchor_portfolio_id_sandboxes_request::offset},
        utility::codec::field{"limit", &list_by_anchor_portfolio_id_sandboxes_request::limit},
        utility::codec::field{"order", &list_by_anchor_portfolio_id_sandboxes_request::order},
        utility::codec::field{"filter", &list_by_anchor_portfolio_id_sandboxes_request::filter},
    };
}

/**
 * @brief The members of list_by_anchor_portfolio_id_sandboxes_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_by_anchor_portfolio_id_sandboxes_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_by_anchor_portfolio_id_sandboxes_response::result},
        utility::codec::field{"sandboxes",
                              &list_by_anchor_portfolio_id_sandboxes_response::sandboxes},
        utility::codec::field{"total", &list_by_anchor_portfolio_id_sandboxes_response::total},
    };
}

/**
 * @brief The members of list_sandbox_versions_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_sandbox_versions_request&) {
    return std::tuple{
        utility::codec::field{"key", &list_sandbox_versions_request::key},
        utility::codec::field{"offset", &list_sandbox_versions_request::offset},
        utility::codec::field{"limit", &list_sandbox_versions_request::limit},
        utility::codec::field{"order", &list_sandbox_versions_request::order},
        utility::codec::field{"filter", &list_sandbox_versions_request::filter},
    };
}

/**
 * @brief The members of list_sandbox_versions_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_sandbox_versions_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_sandbox_versions_response::result},
        utility::codec::field{"versions", &list_sandbox_versions_response::versions},
        utility::codec::field{"total", &list_sandbox_versions_response::total},
    };
}

/**
 * @brief The members of get_sandbox_version_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_sandbox_version_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_sandbox_version_request::key},
    };
}

/**
 * @brief The members of get_sandbox_version_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_sandbox_version_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_sandbox_version_response::result},
        utility::codec::field{"version", &get_sandbox_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::sandbox_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::sandbox_key& v);
    static ores::refdata::messaging::sandbox_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::sandbox_key& v);
    static ores::refdata::messaging::sandbox_key read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::sandbox_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::sandbox_write& v);
    static ores::refdata::messaging::sandbox_write read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::sandbox_write& v);
    static ores::refdata::messaging::sandbox_write read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::sandbox_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::sandbox_change& v);
    static ores::refdata::messaging::sandbox_change read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::sandbox_change& v);
    static ores::refdata::messaging::sandbox_change read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::sandbox_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::sandbox_removal& v);
    static ores::refdata::messaging::sandbox_removal read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::sandbox_removal& v);
    static ores::refdata::messaging::sandbox_removal read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::sandbox_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::sandbox_lookup& v);
    static ores::refdata::messaging::sandbox_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::sandbox_lookup& v);
    static ores::refdata::messaging::sandbox_lookup read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::sandboxes_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::sandboxes_filter& v);
    static ores::refdata::messaging::sandboxes_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::sandboxes_filter& v);
    static ores::refdata::messaging::sandboxes_filter read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::sandbox_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::sandbox_event& v);
    static ores::refdata::messaging::sandbox_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::sandbox_event& v);
    static ores::refdata::messaging::sandbox_event read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::sandbox_version_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::sandbox_version_key& v);
    static ores::refdata::messaging::sandbox_version_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::sandbox_version_key& v);
    static ores::refdata::messaging::sandbox_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::sandbox_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::sandbox_versions_filter& v);
    static ores::refdata::messaging::sandbox_versions_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::sandbox_versions_filter& v);
    static ores::refdata::messaging::sandbox_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::list_sandboxes_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::list_sandboxes_request& v);
    static ores::refdata::messaging::list_sandboxes_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::list_sandboxes_request& v);
    static ores::refdata::messaging::list_sandboxes_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::list_sandboxes_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::list_sandboxes_response& v);
    static ores::refdata::messaging::list_sandboxes_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::list_sandboxes_response& v);
    static ores::refdata::messaging::list_sandboxes_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::get_sandbox_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::get_sandbox_request& v);
    static ores::refdata::messaging::get_sandbox_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::get_sandbox_request& v);
    static ores::refdata::messaging::get_sandbox_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::get_sandbox_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::get_sandbox_response& v);
    static ores::refdata::messaging::get_sandbox_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::get_sandbox_response& v);
    static ores::refdata::messaging::get_sandbox_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::get_many_sandboxes_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::get_many_sandboxes_request& v);
    static ores::refdata::messaging::get_many_sandboxes_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::get_many_sandboxes_request& v);
    static ores::refdata::messaging::get_many_sandboxes_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::get_many_sandboxes_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::get_many_sandboxes_response& v);
    static ores::refdata::messaging::get_many_sandboxes_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_many_sandboxes_response& v);
    static ores::refdata::messaging::get_many_sandboxes_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::put_sandbox_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::put_sandbox_request& v);
    static ores::refdata::messaging::put_sandbox_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::put_sandbox_request& v);
    static ores::refdata::messaging::put_sandbox_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::put_sandbox_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::put_sandbox_response& v);
    static ores::refdata::messaging::put_sandbox_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::put_sandbox_response& v);
    static ores::refdata::messaging::put_sandbox_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::put_many_sandboxes_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::put_many_sandboxes_request& v);
    static ores::refdata::messaging::put_many_sandboxes_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::put_many_sandboxes_request& v);
    static ores::refdata::messaging::put_many_sandboxes_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::put_many_sandboxes_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::put_many_sandboxes_response& v);
    static ores::refdata::messaging::put_many_sandboxes_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_many_sandboxes_response& v);
    static ores::refdata::messaging::put_many_sandboxes_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::delete_sandbox_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::delete_sandbox_request& v);
    static ores::refdata::messaging::delete_sandbox_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::delete_sandbox_request& v);
    static ores::refdata::messaging::delete_sandbox_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::delete_sandbox_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::delete_sandbox_response& v);
    static ores::refdata::messaging::delete_sandbox_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::delete_sandbox_response& v);
    static ores::refdata::messaging::delete_sandbox_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_many_sandboxes_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::delete_many_sandboxes_request& v);
    static ores::refdata::messaging::delete_many_sandboxes_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_many_sandboxes_request& v);
    static ores::refdata::messaging::delete_many_sandboxes_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_many_sandboxes_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_many_sandboxes_response& v);
    static ores::refdata::messaging::delete_many_sandboxes_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_many_sandboxes_response& v);
    static ores::refdata::messaging::delete_many_sandboxes_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_by_anchor_portfolio_id_sandboxes_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_by_anchor_portfolio_id_sandboxes_request& v);
    static ores::refdata::messaging::list_by_anchor_portfolio_id_sandboxes_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_by_anchor_portfolio_id_sandboxes_request& v);
    static ores::refdata::messaging::list_by_anchor_portfolio_id_sandboxes_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_by_anchor_portfolio_id_sandboxes_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_by_anchor_portfolio_id_sandboxes_response& v);
    static ores::refdata::messaging::list_by_anchor_portfolio_id_sandboxes_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::list_by_anchor_portfolio_id_sandboxes_response& v);
    static ores::refdata::messaging::list_by_anchor_portfolio_id_sandboxes_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_sandbox_versions_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::list_sandbox_versions_request& v);
    static ores::refdata::messaging::list_sandbox_versions_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_sandbox_versions_request& v);
    static ores::refdata::messaging::list_sandbox_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_sandbox_versions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_sandbox_versions_response& v);
    static ores::refdata::messaging::list_sandbox_versions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_sandbox_versions_response& v);
    static ores::refdata::messaging::list_sandbox_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::get_sandbox_version_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::get_sandbox_version_request& v);
    static ores::refdata::messaging::get_sandbox_version_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_sandbox_version_request& v);
    static ores::refdata::messaging::get_sandbox_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::get_sandbox_version_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::get_sandbox_version_response& v);
    static ores::refdata::messaging::get_sandbox_version_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_sandbox_version_response& v);
    static ores::refdata::messaging::get_sandbox_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
