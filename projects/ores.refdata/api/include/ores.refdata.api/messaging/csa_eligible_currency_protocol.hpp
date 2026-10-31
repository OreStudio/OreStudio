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
#ifndef ORES_REFDATA_API_MESSAGING_CSA_ELIGIBLE_CURRENCY_PROTOCOL_HPP
#define ORES_REFDATA_API_MESSAGING_CSA_ELIGIBLE_CURRENCY_PROTOCOL_HPP

#include "ores.refdata.api/domain/csa_eligible_currency.hpp"
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

struct csa_eligible_currency_key {
    std::string currency_code;
};

struct csa_eligible_currency_write {
    boost::uuids::uuid id;
    boost::uuids::uuid csa_id;
    std::string currency_code;
    int position;
};

struct csa_eligible_currency_change {
    csa_eligible_currency_write write;
    ores::utility::domain::precondition precondition;
};

struct csa_eligible_currency_removal {
    csa_eligible_currency_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct csa_eligible_currency_lookup {
    csa_eligible_currency_key key;
    std::optional<ores::refdata::domain::csa_eligible_currency> csa_eligible_currency;
};

struct csa_eligible_currencies_filter {
    std::optional<boost::uuids::uuid> csa_id;
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
    std::optional<std::vector<boost::uuids::uuid>> csa_id_one_of;
};

struct csa_eligible_currency_event {
    boost::uuids::uuid event_id;
    csa_eligible_currency_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct csa_eligible_currency_version_key {
    csa_eligible_currency_key csa_eligible_currency;
    std::uint32_t version;
};

struct csa_eligible_currency_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_csa_eligible_currencies_request {
    using response_type = struct list_csa_eligible_currencies_response;
    static constexpr std::string_view nats_subject = "refdata.v1.csa_eligible_currencies.list";
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
    std::optional<csa_eligible_currencies_filter> filter;
    std::optional<std::string> as_of;
};

struct list_csa_eligible_currencies_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::csa_eligible_currency> csa_eligible_currencies;
    std::uint64_t total;
};

struct get_csa_eligible_currency_request {
    using response_type = struct get_csa_eligible_currency_response;
    static constexpr std::string_view nats_subject = "refdata.v1.csa_eligible_currencies.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    csa_eligible_currency_key key;
};

struct get_csa_eligible_currency_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::csa_eligible_currency> csa_eligible_currency;
};

struct get_many_csa_eligible_currencies_request {
    using response_type = struct get_many_csa_eligible_currencies_response;
    static constexpr std::string_view nats_subject = "refdata.v1.csa_eligible_currencies.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<csa_eligible_currency_key> keys;
};

struct get_many_csa_eligible_currencies_response {
    ores::utility::domain::result result;
    std::vector<csa_eligible_currency_lookup> entries;
};

struct put_csa_eligible_currency_request {
    using response_type = struct put_csa_eligible_currency_response;
    static constexpr std::string_view nats_subject = "refdata.v1.csa_eligible_currencies.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    csa_eligible_currency_change change;
    ores::utility::domain::change_intent intent;
};

struct put_csa_eligible_currency_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::csa_eligible_currency> csa_eligible_currency;
};

struct put_many_csa_eligible_currencies_request {
    using response_type = struct put_many_csa_eligible_currencies_response;
    static constexpr std::string_view nats_subject = "refdata.v1.csa_eligible_currencies.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<csa_eligible_currency_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_csa_eligible_currencies_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::csa_eligible_currency> csa_eligible_currencies;
};

struct delete_csa_eligible_currency_request {
    using response_type = struct delete_csa_eligible_currency_response;
    static constexpr std::string_view nats_subject = "refdata.v1.csa_eligible_currencies.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    csa_eligible_currency_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_csa_eligible_currency_response {
    ores::utility::domain::result result;
};

struct delete_many_csa_eligible_currencies_request {
    using response_type = struct delete_many_csa_eligible_currencies_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.csa_eligible_currencies.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<csa_eligible_currency_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_csa_eligible_currencies_response {
    ores::utility::domain::result result;
};

struct list_by_csa_id_csa_eligible_currencies_request {
    using response_type = struct list_by_csa_id_csa_eligible_currencies_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.csa_eligible_currencies.list_by_csa_id";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    boost::uuids::uuid csa_id;
    ores::utility::domain::scope scope = ores::utility::domain::scope::direct;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<csa_eligible_currencies_filter> filter;
};

struct list_by_csa_id_csa_eligible_currencies_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::csa_eligible_currency> csa_eligible_currencies;
    std::uint64_t total;
};

struct list_csa_eligible_currency_versions_request {
    using response_type = struct list_csa_eligible_currency_versions_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.csa_eligible_currencies_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    csa_eligible_currency_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<csa_eligible_currency_versions_filter> filter;
};

struct list_csa_eligible_currency_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::csa_eligible_currency> versions;
    std::uint64_t total;
};

struct get_csa_eligible_currency_version_request {
    using response_type = struct get_csa_eligible_currency_version_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.csa_eligible_currencies_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    csa_eligible_currency_version_key key;
};

struct get_csa_eligible_currency_version_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::csa_eligible_currency> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace csa_eligible_currency_event_subjects {
inline constexpr std::string_view created = "refdata.v1.csa_eligible_currencies_events.created";
inline constexpr std::string_view updated = "refdata.v1.csa_eligible_currencies_events.updated";
inline constexpr std::string_view deleted = "refdata.v1.csa_eligible_currencies_events.deleted";
}
/**
 * @brief The members of csa_eligible_currency_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const csa_eligible_currency_key&) {
    return std::tuple{
        utility::codec::field{"currency_code", &csa_eligible_currency_key::currency_code},
    };
}

/**
 * @brief The members of csa_eligible_currency_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const csa_eligible_currency_write&) {
    return std::tuple{
        utility::codec::field{"id", &csa_eligible_currency_write::id},
        utility::codec::field{"csa_id", &csa_eligible_currency_write::csa_id},
        utility::codec::field{"currency_code", &csa_eligible_currency_write::currency_code},
        utility::codec::field{"position", &csa_eligible_currency_write::position},
    };
}

/**
 * @brief The members of csa_eligible_currency_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const csa_eligible_currency_change&) {
    return std::tuple{
        utility::codec::field{"write", &csa_eligible_currency_change::write},
        utility::codec::field{"precondition", &csa_eligible_currency_change::precondition},
    };
}

/**
 * @brief The members of csa_eligible_currency_removal, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const csa_eligible_currency_removal&) {
    return std::tuple{
        utility::codec::field{"key", &csa_eligible_currency_removal::key},
        utility::codec::field{"precondition", &csa_eligible_currency_removal::precondition},
    };
}

/**
 * @brief The members of csa_eligible_currency_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const csa_eligible_currency_lookup&) {
    return std::tuple{
        utility::codec::field{"key", &csa_eligible_currency_lookup::key},
        utility::codec::field{"csa_eligible_currency",
                              &csa_eligible_currency_lookup::csa_eligible_currency},
    };
}

/**
 * @brief The members of csa_eligible_currencies_filter, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const csa_eligible_currencies_filter&) {
    return std::tuple{
        utility::codec::field{"csa_id", &csa_eligible_currencies_filter::csa_id},
        utility::codec::field{"id_one_of", &csa_eligible_currencies_filter::id_one_of},
        utility::codec::field{"csa_id_one_of", &csa_eligible_currencies_filter::csa_id_one_of},
    };
}

/**
 * @brief The members of csa_eligible_currency_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const csa_eligible_currency_event&) {
    return std::tuple{
        utility::codec::field{"event_id", &csa_eligible_currency_event::event_id},
        utility::codec::field{"key", &csa_eligible_currency_event::key},
        utility::codec::field{"action", &csa_eligible_currency_event::action},
        utility::codec::field{"version", &csa_eligible_currency_event::version},
        utility::codec::field{"occurred_at", &csa_eligible_currency_event::occurred_at},
        utility::codec::field{"correlation_id", &csa_eligible_currency_event::correlation_id},
    };
}

/**
 * @brief The members of csa_eligible_currency_version_key, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const csa_eligible_currency_version_key&) {
    return std::tuple{
        utility::codec::field{"csa_eligible_currency",
                              &csa_eligible_currency_version_key::csa_eligible_currency},
        utility::codec::field{"version", &csa_eligible_currency_version_key::version},
    };
}

/**
 * @brief The members of csa_eligible_currency_versions_filter, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const csa_eligible_currency_versions_filter&) {
    return std::tuple{
        utility::codec::field{"version", &csa_eligible_currency_versions_filter::version},
        utility::codec::field{"from_version", &csa_eligible_currency_versions_filter::from_version},
        utility::codec::field{"to_version", &csa_eligible_currency_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_csa_eligible_currencies_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_csa_eligible_currencies_request&) {
    return std::tuple{
        utility::codec::field{"offset", &list_csa_eligible_currencies_request::offset},
        utility::codec::field{"limit", &list_csa_eligible_currencies_request::limit},
        utility::codec::field{"order", &list_csa_eligible_currencies_request::order},
        utility::codec::field{"filter", &list_csa_eligible_currencies_request::filter},
        utility::codec::field{"as_of", &list_csa_eligible_currencies_request::as_of},
    };
}

/**
 * @brief The members of list_csa_eligible_currencies_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_csa_eligible_currencies_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_csa_eligible_currencies_response::result},
        utility::codec::field{"csa_eligible_currencies",
                              &list_csa_eligible_currencies_response::csa_eligible_currencies},
        utility::codec::field{"total", &list_csa_eligible_currencies_response::total},
    };
}

/**
 * @brief The members of get_csa_eligible_currency_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_csa_eligible_currency_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_csa_eligible_currency_request::key},
    };
}

/**
 * @brief The members of get_csa_eligible_currency_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_csa_eligible_currency_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_csa_eligible_currency_response::result},
        utility::codec::field{"csa_eligible_currency",
                              &get_csa_eligible_currency_response::csa_eligible_currency},
    };
}

/**
 * @brief The members of get_many_csa_eligible_currencies_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_csa_eligible_currencies_request&) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_csa_eligible_currencies_request::keys},
    };
}

/**
 * @brief The members of get_many_csa_eligible_currencies_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_csa_eligible_currencies_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_many_csa_eligible_currencies_response::result},
        utility::codec::field{"entries", &get_many_csa_eligible_currencies_response::entries},
    };
}

/**
 * @brief The members of put_csa_eligible_currency_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_csa_eligible_currency_request&) {
    return std::tuple{
        utility::codec::field{"change", &put_csa_eligible_currency_request::change},
        utility::codec::field{"intent", &put_csa_eligible_currency_request::intent},
    };
}

/**
 * @brief The members of put_csa_eligible_currency_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_csa_eligible_currency_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_csa_eligible_currency_response::result},
        utility::codec::field{"csa_eligible_currency",
                              &put_csa_eligible_currency_response::csa_eligible_currency},
    };
}

/**
 * @brief The members of put_many_csa_eligible_currencies_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_csa_eligible_currencies_request&) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_csa_eligible_currencies_request::changes},
        utility::codec::field{"intent", &put_many_csa_eligible_currencies_request::intent},
    };
}

/**
 * @brief The members of put_many_csa_eligible_currencies_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_csa_eligible_currencies_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_many_csa_eligible_currencies_response::result},
        utility::codec::field{"csa_eligible_currencies",
                              &put_many_csa_eligible_currencies_response::csa_eligible_currencies},
    };
}

/**
 * @brief The members of delete_csa_eligible_currency_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_csa_eligible_currency_request&) {
    return std::tuple{
        utility::codec::field{"removal", &delete_csa_eligible_currency_request::removal},
        utility::codec::field{"intent", &delete_csa_eligible_currency_request::intent},
    };
}

/**
 * @brief The members of delete_csa_eligible_currency_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_csa_eligible_currency_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_csa_eligible_currency_response::result},
    };
}

/**
 * @brief The members of delete_many_csa_eligible_currencies_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_csa_eligible_currencies_request&) {
    return std::tuple{
        utility::codec::field{"removals", &delete_many_csa_eligible_currencies_request::removals},
        utility::codec::field{"intent", &delete_many_csa_eligible_currencies_request::intent},
    };
}

/**
 * @brief The members of delete_many_csa_eligible_currencies_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_csa_eligible_currencies_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_csa_eligible_currencies_response::result},
    };
}

/**
 * @brief The members of list_by_csa_id_csa_eligible_currencies_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_by_csa_id_csa_eligible_currencies_request&) {
    return std::tuple{
        utility::codec::field{"csa_id", &list_by_csa_id_csa_eligible_currencies_request::csa_id},
        utility::codec::field{"scope", &list_by_csa_id_csa_eligible_currencies_request::scope},
        utility::codec::field{"offset", &list_by_csa_id_csa_eligible_currencies_request::offset},
        utility::codec::field{"limit", &list_by_csa_id_csa_eligible_currencies_request::limit},
        utility::codec::field{"order", &list_by_csa_id_csa_eligible_currencies_request::order},
        utility::codec::field{"filter", &list_by_csa_id_csa_eligible_currencies_request::filter},
    };
}

/**
 * @brief The members of list_by_csa_id_csa_eligible_currencies_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_by_csa_id_csa_eligible_currencies_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_by_csa_id_csa_eligible_currencies_response::result},
        utility::codec::field{
            "csa_eligible_currencies",
            &list_by_csa_id_csa_eligible_currencies_response::csa_eligible_currencies},
        utility::codec::field{"total", &list_by_csa_id_csa_eligible_currencies_response::total},
    };
}

/**
 * @brief The members of list_csa_eligible_currency_versions_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_csa_eligible_currency_versions_request&) {
    return std::tuple{
        utility::codec::field{"key", &list_csa_eligible_currency_versions_request::key},
        utility::codec::field{"offset", &list_csa_eligible_currency_versions_request::offset},
        utility::codec::field{"limit", &list_csa_eligible_currency_versions_request::limit},
        utility::codec::field{"order", &list_csa_eligible_currency_versions_request::order},
        utility::codec::field{"filter", &list_csa_eligible_currency_versions_request::filter},
    };
}

/**
 * @brief The members of list_csa_eligible_currency_versions_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_csa_eligible_currency_versions_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_csa_eligible_currency_versions_response::result},
        utility::codec::field{"versions", &list_csa_eligible_currency_versions_response::versions},
        utility::codec::field{"total", &list_csa_eligible_currency_versions_response::total},
    };
}

/**
 * @brief The members of get_csa_eligible_currency_version_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_csa_eligible_currency_version_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_csa_eligible_currency_version_request::key},
    };
}

/**
 * @brief The members of get_csa_eligible_currency_version_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_csa_eligible_currency_version_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_csa_eligible_currency_version_response::result},
        utility::codec::field{"version", &get_csa_eligible_currency_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::csa_eligible_currency_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::csa_eligible_currency_key& v);
    static ores::refdata::messaging::csa_eligible_currency_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::csa_eligible_currency_key& v);
    static ores::refdata::messaging::csa_eligible_currency_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::csa_eligible_currency_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::csa_eligible_currency_write& v);
    static ores::refdata::messaging::csa_eligible_currency_write read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::csa_eligible_currency_write& v);
    static ores::refdata::messaging::csa_eligible_currency_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::csa_eligible_currency_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::csa_eligible_currency_change& v);
    static ores::refdata::messaging::csa_eligible_currency_change read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::csa_eligible_currency_change& v);
    static ores::refdata::messaging::csa_eligible_currency_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::csa_eligible_currency_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::csa_eligible_currency_removal& v);
    static ores::refdata::messaging::csa_eligible_currency_removal read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::csa_eligible_currency_removal& v);
    static ores::refdata::messaging::csa_eligible_currency_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::csa_eligible_currency_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::csa_eligible_currency_lookup& v);
    static ores::refdata::messaging::csa_eligible_currency_lookup read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::csa_eligible_currency_lookup& v);
    static ores::refdata::messaging::csa_eligible_currency_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::csa_eligible_currencies_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::csa_eligible_currencies_filter& v);
    static ores::refdata::messaging::csa_eligible_currencies_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::csa_eligible_currencies_filter& v);
    static ores::refdata::messaging::csa_eligible_currencies_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::csa_eligible_currency_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::csa_eligible_currency_event& v);
    static ores::refdata::messaging::csa_eligible_currency_event read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::csa_eligible_currency_event& v);
    static ores::refdata::messaging::csa_eligible_currency_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::csa_eligible_currency_version_key> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::csa_eligible_currency_version_key& v);
    static ores::refdata::messaging::csa_eligible_currency_version_key
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::csa_eligible_currency_version_key& v);
    static ores::refdata::messaging::csa_eligible_currency_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::csa_eligible_currency_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::csa_eligible_currency_versions_filter& v);
    static ores::refdata::messaging::csa_eligible_currency_versions_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::csa_eligible_currency_versions_filter& v);
    static ores::refdata::messaging::csa_eligible_currency_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_csa_eligible_currencies_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_csa_eligible_currencies_request& v);
    static ores::refdata::messaging::list_csa_eligible_currencies_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_csa_eligible_currencies_request& v);
    static ores::refdata::messaging::list_csa_eligible_currencies_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_csa_eligible_currencies_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_csa_eligible_currencies_response& v);
    static ores::refdata::messaging::list_csa_eligible_currencies_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_csa_eligible_currencies_response& v);
    static ores::refdata::messaging::list_csa_eligible_currencies_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_csa_eligible_currency_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_csa_eligible_currency_request& v);
    static ores::refdata::messaging::get_csa_eligible_currency_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_csa_eligible_currency_request& v);
    static ores::refdata::messaging::get_csa_eligible_currency_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_csa_eligible_currency_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_csa_eligible_currency_response& v);
    static ores::refdata::messaging::get_csa_eligible_currency_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_csa_eligible_currency_response& v);
    static ores::refdata::messaging::get_csa_eligible_currency_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_many_csa_eligible_currencies_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_many_csa_eligible_currencies_request& v);
    static ores::refdata::messaging::get_many_csa_eligible_currencies_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_many_csa_eligible_currencies_request& v);
    static ores::refdata::messaging::get_many_csa_eligible_currencies_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_many_csa_eligible_currencies_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_many_csa_eligible_currencies_response& v);
    static ores::refdata::messaging::get_many_csa_eligible_currencies_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_many_csa_eligible_currencies_response& v);
    static ores::refdata::messaging::get_many_csa_eligible_currencies_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_csa_eligible_currency_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_csa_eligible_currency_request& v);
    static ores::refdata::messaging::put_csa_eligible_currency_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_csa_eligible_currency_request& v);
    static ores::refdata::messaging::put_csa_eligible_currency_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_csa_eligible_currency_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_csa_eligible_currency_response& v);
    static ores::refdata::messaging::put_csa_eligible_currency_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_csa_eligible_currency_response& v);
    static ores::refdata::messaging::put_csa_eligible_currency_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_many_csa_eligible_currencies_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_many_csa_eligible_currencies_request& v);
    static ores::refdata::messaging::put_many_csa_eligible_currencies_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_many_csa_eligible_currencies_request& v);
    static ores::refdata::messaging::put_many_csa_eligible_currencies_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_many_csa_eligible_currencies_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_many_csa_eligible_currencies_response& v);
    static ores::refdata::messaging::put_many_csa_eligible_currencies_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_many_csa_eligible_currencies_response& v);
    static ores::refdata::messaging::put_many_csa_eligible_currencies_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_csa_eligible_currency_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_csa_eligible_currency_request& v);
    static ores::refdata::messaging::delete_csa_eligible_currency_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_csa_eligible_currency_request& v);
    static ores::refdata::messaging::delete_csa_eligible_currency_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_csa_eligible_currency_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_csa_eligible_currency_response& v);
    static ores::refdata::messaging::delete_csa_eligible_currency_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_csa_eligible_currency_response& v);
    static ores::refdata::messaging::delete_csa_eligible_currency_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_many_csa_eligible_currencies_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_many_csa_eligible_currencies_request& v);
    static ores::refdata::messaging::delete_many_csa_eligible_currencies_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_many_csa_eligible_currencies_request& v);
    static ores::refdata::messaging::delete_many_csa_eligible_currencies_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_many_csa_eligible_currencies_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_many_csa_eligible_currencies_response& v);
    static ores::refdata::messaging::delete_many_csa_eligible_currencies_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_many_csa_eligible_currencies_response& v);
    static ores::refdata::messaging::delete_many_csa_eligible_currencies_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_by_csa_id_csa_eligible_currencies_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_by_csa_id_csa_eligible_currencies_request& v);
    static ores::refdata::messaging::list_by_csa_id_csa_eligible_currencies_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::list_by_csa_id_csa_eligible_currencies_request& v);
    static ores::refdata::messaging::list_by_csa_id_csa_eligible_currencies_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_by_csa_id_csa_eligible_currencies_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_by_csa_id_csa_eligible_currencies_response& v);
    static ores::refdata::messaging::list_by_csa_id_csa_eligible_currencies_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::list_by_csa_id_csa_eligible_currencies_response& v);
    static ores::refdata::messaging::list_by_csa_id_csa_eligible_currencies_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_csa_eligible_currency_versions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_csa_eligible_currency_versions_request& v);
    static ores::refdata::messaging::list_csa_eligible_currency_versions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_csa_eligible_currency_versions_request& v);
    static ores::refdata::messaging::list_csa_eligible_currency_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_csa_eligible_currency_versions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_csa_eligible_currency_versions_response& v);
    static ores::refdata::messaging::list_csa_eligible_currency_versions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_csa_eligible_currency_versions_response& v);
    static ores::refdata::messaging::list_csa_eligible_currency_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_csa_eligible_currency_version_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_csa_eligible_currency_version_request& v);
    static ores::refdata::messaging::get_csa_eligible_currency_version_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_csa_eligible_currency_version_request& v);
    static ores::refdata::messaging::get_csa_eligible_currency_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_csa_eligible_currency_version_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_csa_eligible_currency_version_response& v);
    static ores::refdata::messaging::get_csa_eligible_currency_version_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_csa_eligible_currency_version_response& v);
    static ores::refdata::messaging::get_csa_eligible_currency_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
