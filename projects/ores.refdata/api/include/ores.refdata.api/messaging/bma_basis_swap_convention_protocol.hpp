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
#ifndef ORES_REFDATA_API_MESSAGING_BMA_BASIS_SWAP_CONVENTION_PROTOCOL_HPP
#define ORES_REFDATA_API_MESSAGING_BMA_BASIS_SWAP_CONVENTION_PROTOCOL_HPP

#include "ores.refdata.api/domain/bma_basis_swap_convention.hpp"
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

struct bma_basis_swap_convention_key {
    std::string id;
};

struct bma_basis_swap_convention_write {
    std::string id;
    std::string index;
    std::string bma_index;
    std::optional<std::string> bma_payment_calendar;
    std::optional<std::string> bma_payment_convention;
    std::optional<int> bma_payment_lag;
    std::optional<std::string> index_payment_calendar;
    std::optional<std::string> index_payment_convention;
    std::optional<int> index_payment_lag;
    std::optional<int> index_settlement_days;
    std::optional<std::string> index_payment_period;
    std::optional<int> overnight_lockout_days;
    std::optional<std::string> oresmd_uri;
};

struct bma_basis_swap_convention_change {
    bma_basis_swap_convention_write write;
    ores::utility::domain::precondition precondition;
};

struct bma_basis_swap_convention_removal {
    bma_basis_swap_convention_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct bma_basis_swap_convention_lookup {
    bma_basis_swap_convention_key key;
    std::optional<ores::refdata::domain::bma_basis_swap_convention> bma_basis_swap_convention;
};

struct bma_basis_swap_conventions_filter {
    std::optional<std::vector<std::string>> id_one_of;
};

struct bma_basis_swap_convention_event {
    boost::uuids::uuid event_id;
    bma_basis_swap_convention_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct bma_basis_swap_convention_version_key {
    bma_basis_swap_convention_key bma_basis_swap_convention;
    std::uint32_t version;
};

struct bma_basis_swap_convention_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_bma_basis_swap_conventions_request {
    using response_type = struct list_bma_basis_swap_conventions_response;
    static constexpr std::string_view nats_subject = "refdata.v1.bma_basis_swap_conventions.list";
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
    std::optional<bma_basis_swap_conventions_filter> filter;
    std::optional<std::string> as_of;
};

struct list_bma_basis_swap_conventions_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::bma_basis_swap_convention> bma_basis_swap_conventions;
    std::uint64_t total;
};

struct get_bma_basis_swap_convention_request {
    using response_type = struct get_bma_basis_swap_convention_response;
    static constexpr std::string_view nats_subject = "refdata.v1.bma_basis_swap_conventions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    bma_basis_swap_convention_key key;
};

struct get_bma_basis_swap_convention_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::bma_basis_swap_convention> bma_basis_swap_convention;
};

struct get_many_bma_basis_swap_conventions_request {
    using response_type = struct get_many_bma_basis_swap_conventions_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.bma_basis_swap_conventions.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<bma_basis_swap_convention_key> keys;
};

struct get_many_bma_basis_swap_conventions_response {
    ores::utility::domain::result result;
    std::vector<bma_basis_swap_convention_lookup> entries;
};

struct put_bma_basis_swap_convention_request {
    using response_type = struct put_bma_basis_swap_convention_response;
    static constexpr std::string_view nats_subject = "refdata.v1.bma_basis_swap_conventions.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    bma_basis_swap_convention_change change;
    ores::utility::domain::change_intent intent;
};

struct put_bma_basis_swap_convention_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::bma_basis_swap_convention> bma_basis_swap_convention;
};

struct put_many_bma_basis_swap_conventions_request {
    using response_type = struct put_many_bma_basis_swap_conventions_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.bma_basis_swap_conventions.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<bma_basis_swap_convention_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_bma_basis_swap_conventions_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::bma_basis_swap_convention> bma_basis_swap_conventions;
};

struct delete_bma_basis_swap_convention_request {
    using response_type = struct delete_bma_basis_swap_convention_response;
    static constexpr std::string_view nats_subject = "refdata.v1.bma_basis_swap_conventions.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    bma_basis_swap_convention_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_bma_basis_swap_convention_response {
    ores::utility::domain::result result;
};

struct delete_many_bma_basis_swap_conventions_request {
    using response_type = struct delete_many_bma_basis_swap_conventions_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.bma_basis_swap_conventions.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<bma_basis_swap_convention_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_bma_basis_swap_conventions_response {
    ores::utility::domain::result result;
};

struct list_bma_basis_swap_convention_versions_request {
    using response_type = struct list_bma_basis_swap_convention_versions_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.bma_basis_swap_conventions_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    bma_basis_swap_convention_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<bma_basis_swap_convention_versions_filter> filter;
};

struct list_bma_basis_swap_convention_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::bma_basis_swap_convention> versions;
    std::uint64_t total;
};

struct get_bma_basis_swap_convention_version_request {
    using response_type = struct get_bma_basis_swap_convention_version_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.bma_basis_swap_conventions_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    bma_basis_swap_convention_version_key key;
};

struct get_bma_basis_swap_convention_version_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::bma_basis_swap_convention> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace bma_basis_swap_convention_event_subjects {
inline constexpr std::string_view created = "refdata.v1.bma_basis_swap_conventions_events.created";
inline constexpr std::string_view updated = "refdata.v1.bma_basis_swap_conventions_events.updated";
inline constexpr std::string_view deleted = "refdata.v1.bma_basis_swap_conventions_events.deleted";
}
/**
 * @brief The members of bma_basis_swap_convention_key, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const bma_basis_swap_convention_key&) {
    return std::tuple{
        utility::codec::field{"id", &bma_basis_swap_convention_key::id},
    };
}

/**
 * @brief The members of bma_basis_swap_convention_write, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const bma_basis_swap_convention_write&) {
    return std::tuple{
        utility::codec::field{"id", &bma_basis_swap_convention_write::id},
        utility::codec::field{"index", &bma_basis_swap_convention_write::index},
        utility::codec::field{"bma_index", &bma_basis_swap_convention_write::bma_index},
        utility::codec::field{"bma_payment_calendar",
                              &bma_basis_swap_convention_write::bma_payment_calendar},
        utility::codec::field{"bma_payment_convention",
                              &bma_basis_swap_convention_write::bma_payment_convention},
        utility::codec::field{"bma_payment_lag", &bma_basis_swap_convention_write::bma_payment_lag},
        utility::codec::field{"index_payment_calendar",
                              &bma_basis_swap_convention_write::index_payment_calendar},
        utility::codec::field{"index_payment_convention",
                              &bma_basis_swap_convention_write::index_payment_convention},
        utility::codec::field{"index_payment_lag",
                              &bma_basis_swap_convention_write::index_payment_lag},
        utility::codec::field{"index_settlement_days",
                              &bma_basis_swap_convention_write::index_settlement_days},
        utility::codec::field{"index_payment_period",
                              &bma_basis_swap_convention_write::index_payment_period},
        utility::codec::field{"overnight_lockout_days",
                              &bma_basis_swap_convention_write::overnight_lockout_days},
        utility::codec::field{"oresmd_uri", &bma_basis_swap_convention_write::oresmd_uri},
    };
}

/**
 * @brief The members of bma_basis_swap_convention_change, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const bma_basis_swap_convention_change&) {
    return std::tuple{
        utility::codec::field{"write", &bma_basis_swap_convention_change::write},
        utility::codec::field{"precondition", &bma_basis_swap_convention_change::precondition},
    };
}

/**
 * @brief The members of bma_basis_swap_convention_removal, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const bma_basis_swap_convention_removal&) {
    return std::tuple{
        utility::codec::field{"key", &bma_basis_swap_convention_removal::key},
        utility::codec::field{"precondition", &bma_basis_swap_convention_removal::precondition},
    };
}

/**
 * @brief The members of bma_basis_swap_convention_lookup, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const bma_basis_swap_convention_lookup&) {
    return std::tuple{
        utility::codec::field{"key", &bma_basis_swap_convention_lookup::key},
        utility::codec::field{"bma_basis_swap_convention",
                              &bma_basis_swap_convention_lookup::bma_basis_swap_convention},
    };
}

/**
 * @brief The members of bma_basis_swap_conventions_filter, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const bma_basis_swap_conventions_filter&) {
    return std::tuple{
        utility::codec::field{"id_one_of", &bma_basis_swap_conventions_filter::id_one_of},
    };
}

/**
 * @brief The members of bma_basis_swap_convention_event, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const bma_basis_swap_convention_event&) {
    return std::tuple{
        utility::codec::field{"event_id", &bma_basis_swap_convention_event::event_id},
        utility::codec::field{"key", &bma_basis_swap_convention_event::key},
        utility::codec::field{"action", &bma_basis_swap_convention_event::action},
        utility::codec::field{"version", &bma_basis_swap_convention_event::version},
        utility::codec::field{"occurred_at", &bma_basis_swap_convention_event::occurred_at},
        utility::codec::field{"correlation_id", &bma_basis_swap_convention_event::correlation_id},
    };
}

/**
 * @brief The members of bma_basis_swap_convention_version_key, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const bma_basis_swap_convention_version_key&) {
    return std::tuple{
        utility::codec::field{"bma_basis_swap_convention",
                              &bma_basis_swap_convention_version_key::bma_basis_swap_convention},
        utility::codec::field{"version", &bma_basis_swap_convention_version_key::version},
    };
}

/**
 * @brief The members of bma_basis_swap_convention_versions_filter, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const bma_basis_swap_convention_versions_filter&) {
    return std::tuple{
        utility::codec::field{"version", &bma_basis_swap_convention_versions_filter::version},
        utility::codec::field{"from_version",
                              &bma_basis_swap_convention_versions_filter::from_version},
        utility::codec::field{"to_version", &bma_basis_swap_convention_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_bma_basis_swap_conventions_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_bma_basis_swap_conventions_request&) {
    return std::tuple{
        utility::codec::field{"offset", &list_bma_basis_swap_conventions_request::offset},
        utility::codec::field{"limit", &list_bma_basis_swap_conventions_request::limit},
        utility::codec::field{"order", &list_bma_basis_swap_conventions_request::order},
        utility::codec::field{"filter", &list_bma_basis_swap_conventions_request::filter},
        utility::codec::field{"as_of", &list_bma_basis_swap_conventions_request::as_of},
    };
}

/**
 * @brief The members of list_bma_basis_swap_conventions_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_bma_basis_swap_conventions_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_bma_basis_swap_conventions_response::result},
        utility::codec::field{
            "bma_basis_swap_conventions",
            &list_bma_basis_swap_conventions_response::bma_basis_swap_conventions},
        utility::codec::field{"total", &list_bma_basis_swap_conventions_response::total},
    };
}

/**
 * @brief The members of get_bma_basis_swap_convention_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_bma_basis_swap_convention_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_bma_basis_swap_convention_request::key},
    };
}

/**
 * @brief The members of get_bma_basis_swap_convention_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_bma_basis_swap_convention_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_bma_basis_swap_convention_response::result},
        utility::codec::field{"bma_basis_swap_convention",
                              &get_bma_basis_swap_convention_response::bma_basis_swap_convention},
    };
}

/**
 * @brief The members of get_many_bma_basis_swap_conventions_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_bma_basis_swap_conventions_request&) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_bma_basis_swap_conventions_request::keys},
    };
}

/**
 * @brief The members of get_many_bma_basis_swap_conventions_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_bma_basis_swap_conventions_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_many_bma_basis_swap_conventions_response::result},
        utility::codec::field{"entries", &get_many_bma_basis_swap_conventions_response::entries},
    };
}

/**
 * @brief The members of put_bma_basis_swap_convention_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_bma_basis_swap_convention_request&) {
    return std::tuple{
        utility::codec::field{"change", &put_bma_basis_swap_convention_request::change},
        utility::codec::field{"intent", &put_bma_basis_swap_convention_request::intent},
    };
}

/**
 * @brief The members of put_bma_basis_swap_convention_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_bma_basis_swap_convention_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_bma_basis_swap_convention_response::result},
        utility::codec::field{"bma_basis_swap_convention",
                              &put_bma_basis_swap_convention_response::bma_basis_swap_convention},
    };
}

/**
 * @brief The members of put_many_bma_basis_swap_conventions_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_bma_basis_swap_conventions_request&) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_bma_basis_swap_conventions_request::changes},
        utility::codec::field{"intent", &put_many_bma_basis_swap_conventions_request::intent},
    };
}

/**
 * @brief The members of put_many_bma_basis_swap_conventions_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_bma_basis_swap_conventions_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_many_bma_basis_swap_conventions_response::result},
        utility::codec::field{
            "bma_basis_swap_conventions",
            &put_many_bma_basis_swap_conventions_response::bma_basis_swap_conventions},
    };
}

/**
 * @brief The members of delete_bma_basis_swap_convention_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_bma_basis_swap_convention_request&) {
    return std::tuple{
        utility::codec::field{"removal", &delete_bma_basis_swap_convention_request::removal},
        utility::codec::field{"intent", &delete_bma_basis_swap_convention_request::intent},
    };
}

/**
 * @brief The members of delete_bma_basis_swap_convention_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_bma_basis_swap_convention_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_bma_basis_swap_convention_response::result},
    };
}

/**
 * @brief The members of delete_many_bma_basis_swap_conventions_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_bma_basis_swap_conventions_request&) {
    return std::tuple{
        utility::codec::field{"removals",
                              &delete_many_bma_basis_swap_conventions_request::removals},
        utility::codec::field{"intent", &delete_many_bma_basis_swap_conventions_request::intent},
    };
}

/**
 * @brief The members of delete_many_bma_basis_swap_conventions_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_bma_basis_swap_conventions_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_bma_basis_swap_conventions_response::result},
    };
}

/**
 * @brief The members of list_bma_basis_swap_convention_versions_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_bma_basis_swap_convention_versions_request&) {
    return std::tuple{
        utility::codec::field{"key", &list_bma_basis_swap_convention_versions_request::key},
        utility::codec::field{"offset", &list_bma_basis_swap_convention_versions_request::offset},
        utility::codec::field{"limit", &list_bma_basis_swap_convention_versions_request::limit},
        utility::codec::field{"order", &list_bma_basis_swap_convention_versions_request::order},
        utility::codec::field{"filter", &list_bma_basis_swap_convention_versions_request::filter},
    };
}

/**
 * @brief The members of list_bma_basis_swap_convention_versions_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_bma_basis_swap_convention_versions_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_bma_basis_swap_convention_versions_response::result},
        utility::codec::field{"versions",
                              &list_bma_basis_swap_convention_versions_response::versions},
        utility::codec::field{"total", &list_bma_basis_swap_convention_versions_response::total},
    };
}

/**
 * @brief The members of get_bma_basis_swap_convention_version_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_bma_basis_swap_convention_version_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_bma_basis_swap_convention_version_request::key},
    };
}

/**
 * @brief The members of get_bma_basis_swap_convention_version_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_bma_basis_swap_convention_version_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_bma_basis_swap_convention_version_response::result},
        utility::codec::field{"version", &get_bma_basis_swap_convention_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::bma_basis_swap_convention_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::bma_basis_swap_convention_key& v);
    static ores::refdata::messaging::bma_basis_swap_convention_key read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::bma_basis_swap_convention_key& v);
    static ores::refdata::messaging::bma_basis_swap_convention_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::bma_basis_swap_convention_write> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::bma_basis_swap_convention_write& v);
    static ores::refdata::messaging::bma_basis_swap_convention_write
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::bma_basis_swap_convention_write& v);
    static ores::refdata::messaging::bma_basis_swap_convention_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::bma_basis_swap_convention_change> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::bma_basis_swap_convention_change& v);
    static ores::refdata::messaging::bma_basis_swap_convention_change
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::bma_basis_swap_convention_change& v);
    static ores::refdata::messaging::bma_basis_swap_convention_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::bma_basis_swap_convention_removal> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::bma_basis_swap_convention_removal& v);
    static ores::refdata::messaging::bma_basis_swap_convention_removal
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::bma_basis_swap_convention_removal& v);
    static ores::refdata::messaging::bma_basis_swap_convention_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::bma_basis_swap_convention_lookup> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::bma_basis_swap_convention_lookup& v);
    static ores::refdata::messaging::bma_basis_swap_convention_lookup
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::bma_basis_swap_convention_lookup& v);
    static ores::refdata::messaging::bma_basis_swap_convention_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::bma_basis_swap_conventions_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::bma_basis_swap_conventions_filter& v);
    static ores::refdata::messaging::bma_basis_swap_conventions_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::bma_basis_swap_conventions_filter& v);
    static ores::refdata::messaging::bma_basis_swap_conventions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::bma_basis_swap_convention_event> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::bma_basis_swap_convention_event& v);
    static ores::refdata::messaging::bma_basis_swap_convention_event
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::bma_basis_swap_convention_event& v);
    static ores::refdata::messaging::bma_basis_swap_convention_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::bma_basis_swap_convention_version_key> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::bma_basis_swap_convention_version_key& v);
    static ores::refdata::messaging::bma_basis_swap_convention_version_key
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::bma_basis_swap_convention_version_key& v);
    static ores::refdata::messaging::bma_basis_swap_convention_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::bma_basis_swap_convention_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::bma_basis_swap_convention_versions_filter& v);
    static ores::refdata::messaging::bma_basis_swap_convention_versions_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::bma_basis_swap_convention_versions_filter& v);
    static ores::refdata::messaging::bma_basis_swap_convention_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_bma_basis_swap_conventions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_bma_basis_swap_conventions_request& v);
    static ores::refdata::messaging::list_bma_basis_swap_conventions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_bma_basis_swap_conventions_request& v);
    static ores::refdata::messaging::list_bma_basis_swap_conventions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_bma_basis_swap_conventions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_bma_basis_swap_conventions_response& v);
    static ores::refdata::messaging::list_bma_basis_swap_conventions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_bma_basis_swap_conventions_response& v);
    static ores::refdata::messaging::list_bma_basis_swap_conventions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_bma_basis_swap_convention_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_bma_basis_swap_convention_request& v);
    static ores::refdata::messaging::get_bma_basis_swap_convention_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_bma_basis_swap_convention_request& v);
    static ores::refdata::messaging::get_bma_basis_swap_convention_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_bma_basis_swap_convention_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_bma_basis_swap_convention_response& v);
    static ores::refdata::messaging::get_bma_basis_swap_convention_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_bma_basis_swap_convention_response& v);
    static ores::refdata::messaging::get_bma_basis_swap_convention_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_many_bma_basis_swap_conventions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_many_bma_basis_swap_conventions_request& v);
    static ores::refdata::messaging::get_many_bma_basis_swap_conventions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_many_bma_basis_swap_conventions_request& v);
    static ores::refdata::messaging::get_many_bma_basis_swap_conventions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_many_bma_basis_swap_conventions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_many_bma_basis_swap_conventions_response& v);
    static ores::refdata::messaging::get_many_bma_basis_swap_conventions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_many_bma_basis_swap_conventions_response& v);
    static ores::refdata::messaging::get_many_bma_basis_swap_conventions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_bma_basis_swap_convention_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_bma_basis_swap_convention_request& v);
    static ores::refdata::messaging::put_bma_basis_swap_convention_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_bma_basis_swap_convention_request& v);
    static ores::refdata::messaging::put_bma_basis_swap_convention_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_bma_basis_swap_convention_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_bma_basis_swap_convention_response& v);
    static ores::refdata::messaging::put_bma_basis_swap_convention_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_bma_basis_swap_convention_response& v);
    static ores::refdata::messaging::put_bma_basis_swap_convention_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_many_bma_basis_swap_conventions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_many_bma_basis_swap_conventions_request& v);
    static ores::refdata::messaging::put_many_bma_basis_swap_conventions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_many_bma_basis_swap_conventions_request& v);
    static ores::refdata::messaging::put_many_bma_basis_swap_conventions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_many_bma_basis_swap_conventions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_many_bma_basis_swap_conventions_response& v);
    static ores::refdata::messaging::put_many_bma_basis_swap_conventions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_many_bma_basis_swap_conventions_response& v);
    static ores::refdata::messaging::put_many_bma_basis_swap_conventions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_bma_basis_swap_convention_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_bma_basis_swap_convention_request& v);
    static ores::refdata::messaging::delete_bma_basis_swap_convention_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_bma_basis_swap_convention_request& v);
    static ores::refdata::messaging::delete_bma_basis_swap_convention_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_bma_basis_swap_convention_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_bma_basis_swap_convention_response& v);
    static ores::refdata::messaging::delete_bma_basis_swap_convention_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_bma_basis_swap_convention_response& v);
    static ores::refdata::messaging::delete_bma_basis_swap_convention_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_many_bma_basis_swap_conventions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_many_bma_basis_swap_conventions_request& v);
    static ores::refdata::messaging::delete_many_bma_basis_swap_conventions_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::delete_many_bma_basis_swap_conventions_request& v);
    static ores::refdata::messaging::delete_many_bma_basis_swap_conventions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_many_bma_basis_swap_conventions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_many_bma_basis_swap_conventions_response& v);
    static ores::refdata::messaging::delete_many_bma_basis_swap_conventions_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::delete_many_bma_basis_swap_conventions_response& v);
    static ores::refdata::messaging::delete_many_bma_basis_swap_conventions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_bma_basis_swap_convention_versions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_bma_basis_swap_convention_versions_request& v);
    static ores::refdata::messaging::list_bma_basis_swap_convention_versions_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::list_bma_basis_swap_convention_versions_request& v);
    static ores::refdata::messaging::list_bma_basis_swap_convention_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_bma_basis_swap_convention_versions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_bma_basis_swap_convention_versions_response& v);
    static ores::refdata::messaging::list_bma_basis_swap_convention_versions_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::list_bma_basis_swap_convention_versions_response& v);
    static ores::refdata::messaging::list_bma_basis_swap_convention_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_bma_basis_swap_convention_version_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_bma_basis_swap_convention_version_request& v);
    static ores::refdata::messaging::get_bma_basis_swap_convention_version_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_bma_basis_swap_convention_version_request& v);
    static ores::refdata::messaging::get_bma_basis_swap_convention_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_bma_basis_swap_convention_version_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_bma_basis_swap_convention_version_response& v);
    static ores::refdata::messaging::get_bma_basis_swap_convention_version_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::get_bma_basis_swap_convention_version_response& v);
    static ores::refdata::messaging::get_bma_basis_swap_convention_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
