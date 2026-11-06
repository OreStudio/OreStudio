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
#ifndef ORES_MARKETDATA_API_MESSAGING_MARKET_DATA_VENDOR_INSTRUMENT_PROTOCOL_HPP
#define ORES_MARKETDATA_API_MESSAGING_MARKET_DATA_VENDOR_INSTRUMENT_PROTOCOL_HPP

#include "ores.marketdata.api/domain/market_data_vendor_instrument.hpp"
#include "ores.marketdata.api/export.hpp"
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

namespace ores::marketdata::messaging {

struct market_data_vendor_instrument_key {
    std::string external_id;

    friend bool operator==(const market_data_vendor_instrument_key&,
                           const market_data_vendor_instrument_key&) = default;
};

struct market_data_vendor_instrument_write {
    boost::uuids::uuid id;
    std::string provider_code;
    std::string id_scheme;
    std::string external_id;
    std::string field;
    bool minted;
    std::string deviation_note;

    friend bool operator==(const market_data_vendor_instrument_write&,
                           const market_data_vendor_instrument_write&) = default;
};

struct market_data_vendor_instrument_change {
    market_data_vendor_instrument_write write;
    ores::utility::domain::precondition precondition;

    friend bool operator==(const market_data_vendor_instrument_change&,
                           const market_data_vendor_instrument_change&) = default;
};

struct market_data_vendor_instrument_removal {
    market_data_vendor_instrument_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;

    friend bool operator==(const market_data_vendor_instrument_removal&,
                           const market_data_vendor_instrument_removal&) = default;
};

struct market_data_vendor_instrument_lookup {
    market_data_vendor_instrument_key key;
    std::optional<ores::marketdata::domain::market_data_vendor_instrument>
        market_data_vendor_instrument;

    friend bool operator==(const market_data_vendor_instrument_lookup&,
                           const market_data_vendor_instrument_lookup&) = default;
};

struct market_data_vendor_instruments_filter {
    std::optional<std::string> provider_code;
    std::optional<std::string> id_scheme;
    std::optional<std::string> field;
    std::optional<bool> minted;
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
    std::optional<std::vector<std::string>> provider_code_one_of;
    std::optional<std::vector<std::string>> id_scheme_one_of;
    std::optional<std::vector<std::string>> field_one_of;
    std::optional<std::vector<bool>> minted_one_of;
    std::optional<std::string> search;

    friend bool operator==(const market_data_vendor_instruments_filter&,
                           const market_data_vendor_instruments_filter&) = default;
};

struct market_data_vendor_instrument_event {
    boost::uuids::uuid event_id;
    market_data_vendor_instrument_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;

    friend bool operator==(const market_data_vendor_instrument_event&,
                           const market_data_vendor_instrument_event&) = default;
};

struct market_data_vendor_instrument_version_key {
    market_data_vendor_instrument_key market_data_vendor_instrument;
    std::uint32_t version;

    friend bool operator==(const market_data_vendor_instrument_version_key&,
                           const market_data_vendor_instrument_version_key&) = default;
};

struct market_data_vendor_instrument_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;

    friend bool operator==(const market_data_vendor_instrument_versions_filter&,
                           const market_data_vendor_instrument_versions_filter&) = default;
};

struct list_market_data_vendor_instruments_request {
    using response_type = struct list_market_data_vendor_instruments_response;
    static constexpr std::string_view nats_subject =
        "marketdata.v1.market_data_vendor_instruments.list";
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
    std::optional<market_data_vendor_instruments_filter> filter;
    std::optional<std::string> as_of;

    friend bool operator==(const list_market_data_vendor_instruments_request&,
                           const list_market_data_vendor_instruments_request&) = default;
};

struct list_market_data_vendor_instruments_response {
    ores::utility::domain::result result;
    std::vector<ores::marketdata::domain::market_data_vendor_instrument> vendor_instruments;
    std::uint64_t total;

    friend bool operator==(const list_market_data_vendor_instruments_response&,
                           const list_market_data_vendor_instruments_response&) = default;
};

struct get_market_data_vendor_instrument_request {
    using response_type = struct get_market_data_vendor_instrument_response;
    static constexpr std::string_view nats_subject =
        "marketdata.v1.market_data_vendor_instruments.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    market_data_vendor_instrument_key key;

    friend bool operator==(const get_market_data_vendor_instrument_request&,
                           const get_market_data_vendor_instrument_request&) = default;
};

struct get_market_data_vendor_instrument_response {
    ores::utility::domain::result result;
    std::optional<ores::marketdata::domain::market_data_vendor_instrument>
        market_data_vendor_instrument;

    friend bool operator==(const get_market_data_vendor_instrument_response&,
                           const get_market_data_vendor_instrument_response&) = default;
};

struct get_many_market_data_vendor_instruments_request {
    using response_type = struct get_many_market_data_vendor_instruments_response;
    static constexpr std::string_view nats_subject =
        "marketdata.v1.market_data_vendor_instruments.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<market_data_vendor_instrument_key> keys;

    friend bool operator==(const get_many_market_data_vendor_instruments_request&,
                           const get_many_market_data_vendor_instruments_request&) = default;
};

struct get_many_market_data_vendor_instruments_response {
    ores::utility::domain::result result;
    std::vector<market_data_vendor_instrument_lookup> entries;

    friend bool operator==(const get_many_market_data_vendor_instruments_response&,
                           const get_many_market_data_vendor_instruments_response&) = default;
};

struct put_market_data_vendor_instrument_request {
    using response_type = struct put_market_data_vendor_instrument_response;
    static constexpr std::string_view nats_subject =
        "marketdata.v1.market_data_vendor_instruments.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    market_data_vendor_instrument_change change;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_market_data_vendor_instrument_request&,
                           const put_market_data_vendor_instrument_request&) = default;
};

struct put_market_data_vendor_instrument_response {
    ores::utility::domain::result result;
    std::optional<ores::marketdata::domain::market_data_vendor_instrument>
        market_data_vendor_instrument;

    friend bool operator==(const put_market_data_vendor_instrument_response&,
                           const put_market_data_vendor_instrument_response&) = default;
};

struct put_many_market_data_vendor_instruments_request {
    using response_type = struct put_many_market_data_vendor_instruments_response;
    static constexpr std::string_view nats_subject =
        "marketdata.v1.market_data_vendor_instruments.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<market_data_vendor_instrument_change> changes;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_many_market_data_vendor_instruments_request&,
                           const put_many_market_data_vendor_instruments_request&) = default;
};

struct put_many_market_data_vendor_instruments_response {
    ores::utility::domain::result result;
    std::vector<ores::marketdata::domain::market_data_vendor_instrument> vendor_instruments;

    friend bool operator==(const put_many_market_data_vendor_instruments_response&,
                           const put_many_market_data_vendor_instruments_response&) = default;
};

struct delete_market_data_vendor_instrument_request {
    using response_type = struct delete_market_data_vendor_instrument_response;
    static constexpr std::string_view nats_subject =
        "marketdata.v1.market_data_vendor_instruments.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    market_data_vendor_instrument_removal removal;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_market_data_vendor_instrument_request&,
                           const delete_market_data_vendor_instrument_request&) = default;
};

struct delete_market_data_vendor_instrument_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_market_data_vendor_instrument_response&,
                           const delete_market_data_vendor_instrument_response&) = default;
};

struct delete_many_market_data_vendor_instruments_request {
    using response_type = struct delete_many_market_data_vendor_instruments_response;
    static constexpr std::string_view nats_subject =
        "marketdata.v1.market_data_vendor_instruments.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<market_data_vendor_instrument_removal> removals;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_many_market_data_vendor_instruments_request&,
                           const delete_many_market_data_vendor_instruments_request&) = default;
};

struct delete_many_market_data_vendor_instruments_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_many_market_data_vendor_instruments_response&,
                           const delete_many_market_data_vendor_instruments_response&) = default;
};

struct list_market_data_vendor_instrument_versions_request {
    using response_type = struct list_market_data_vendor_instrument_versions_response;
    static constexpr std::string_view nats_subject =
        "marketdata.v1.market_data_vendor_instruments_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    market_data_vendor_instrument_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<market_data_vendor_instrument_versions_filter> filter;

    friend bool operator==(const list_market_data_vendor_instrument_versions_request&,
                           const list_market_data_vendor_instrument_versions_request&) = default;
};

struct list_market_data_vendor_instrument_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::marketdata::domain::market_data_vendor_instrument> versions;
    std::uint64_t total;

    friend bool operator==(const list_market_data_vendor_instrument_versions_response&,
                           const list_market_data_vendor_instrument_versions_response&) = default;
};

struct get_market_data_vendor_instrument_version_request {
    using response_type = struct get_market_data_vendor_instrument_version_response;
    static constexpr std::string_view nats_subject =
        "marketdata.v1.market_data_vendor_instruments_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    market_data_vendor_instrument_version_key key;

    friend bool operator==(const get_market_data_vendor_instrument_version_request&,
                           const get_market_data_vendor_instrument_version_request&) = default;
};

struct get_market_data_vendor_instrument_version_response {
    ores::utility::domain::result result;
    std::optional<ores::marketdata::domain::market_data_vendor_instrument> version;

    friend bool operator==(const get_market_data_vendor_instrument_version_response&,
                           const get_market_data_vendor_instrument_version_response&) = default;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace market_data_vendor_instrument_event_subjects {
inline constexpr std::string_view created =
    "marketdata.v1.events.market_data_vendor_instruments.created";
inline constexpr std::string_view updated =
    "marketdata.v1.events.market_data_vendor_instruments.updated";
inline constexpr std::string_view deleted =
    "marketdata.v1.events.market_data_vendor_instruments.deleted";
}
/**
 * @brief The members of market_data_vendor_instrument_key, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<market_data_vendor_instrument_key>) {
    return std::tuple{
        utility::codec::field{"external_id", &market_data_vendor_instrument_key::external_id},
    };
}

/**
 * @brief The members of market_data_vendor_instrument_write, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<market_data_vendor_instrument_write>) {
    return std::tuple{
        utility::codec::field{"id", &market_data_vendor_instrument_write::id},
        utility::codec::field{"provider_code", &market_data_vendor_instrument_write::provider_code},
        utility::codec::field{"id_scheme", &market_data_vendor_instrument_write::id_scheme},
        utility::codec::field{"external_id", &market_data_vendor_instrument_write::external_id},
        utility::codec::field{"field", &market_data_vendor_instrument_write::field},
        utility::codec::field{"minted", &market_data_vendor_instrument_write::minted},
        utility::codec::field{"deviation_note",
                              &market_data_vendor_instrument_write::deviation_note},
    };
}

/**
 * @brief The members of market_data_vendor_instrument_change, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<market_data_vendor_instrument_change>) {
    return std::tuple{
        utility::codec::field{"write", &market_data_vendor_instrument_change::write},
        utility::codec::field{"precondition", &market_data_vendor_instrument_change::precondition},
    };
}

/**
 * @brief The members of market_data_vendor_instrument_removal, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<market_data_vendor_instrument_removal>) {
    return std::tuple{
        utility::codec::field{"key", &market_data_vendor_instrument_removal::key},
        utility::codec::field{"precondition", &market_data_vendor_instrument_removal::precondition},
    };
}

/**
 * @brief The members of market_data_vendor_instrument_lookup, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<market_data_vendor_instrument_lookup>) {
    return std::tuple{
        utility::codec::field{"key", &market_data_vendor_instrument_lookup::key},
        utility::codec::field{"market_data_vendor_instrument",
                              &market_data_vendor_instrument_lookup::market_data_vendor_instrument},
    };
}

/**
 * @brief The members of market_data_vendor_instruments_filter, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<market_data_vendor_instruments_filter>) {
    return std::tuple{
        utility::codec::field{"provider_code",
                              &market_data_vendor_instruments_filter::provider_code},
        utility::codec::field{"id_scheme", &market_data_vendor_instruments_filter::id_scheme},
        utility::codec::field{"field", &market_data_vendor_instruments_filter::field},
        utility::codec::field{"minted", &market_data_vendor_instruments_filter::minted},
        utility::codec::field{"id_one_of", &market_data_vendor_instruments_filter::id_one_of},
        utility::codec::field{"provider_code_one_of",
                              &market_data_vendor_instruments_filter::provider_code_one_of},
        utility::codec::field{"id_scheme_one_of",
                              &market_data_vendor_instruments_filter::id_scheme_one_of},
        utility::codec::field{"field_one_of", &market_data_vendor_instruments_filter::field_one_of},
        utility::codec::field{"minted_one_of",
                              &market_data_vendor_instruments_filter::minted_one_of},
        utility::codec::field{"search", &market_data_vendor_instruments_filter::search},
    };
}

/**
 * @brief The members of market_data_vendor_instrument_event, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<market_data_vendor_instrument_event>) {
    return std::tuple{
        utility::codec::field{"event_id", &market_data_vendor_instrument_event::event_id},
        utility::codec::field{"key", &market_data_vendor_instrument_event::key},
        utility::codec::field{"action", &market_data_vendor_instrument_event::action},
        utility::codec::field{"version", &market_data_vendor_instrument_event::version},
        utility::codec::field{"occurred_at", &market_data_vendor_instrument_event::occurred_at},
        utility::codec::field{"correlation_id",
                              &market_data_vendor_instrument_event::correlation_id},
    };
}

/**
 * @brief The members of market_data_vendor_instrument_version_key, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<market_data_vendor_instrument_version_key>) {
    return std::tuple{
        utility::codec::field{
            "market_data_vendor_instrument",
            &market_data_vendor_instrument_version_key::market_data_vendor_instrument},
        utility::codec::field{"version", &market_data_vendor_instrument_version_key::version},
    };
}

/**
 * @brief The members of market_data_vendor_instrument_versions_filter, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<market_data_vendor_instrument_versions_filter>) {
    return std::tuple{
        utility::codec::field{"version", &market_data_vendor_instrument_versions_filter::version},
        utility::codec::field{"from_version",
                              &market_data_vendor_instrument_versions_filter::from_version},
        utility::codec::field{"to_version",
                              &market_data_vendor_instrument_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_market_data_vendor_instruments_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_market_data_vendor_instruments_request>) {
    return std::tuple{
        utility::codec::field{"offset", &list_market_data_vendor_instruments_request::offset},
        utility::codec::field{"limit", &list_market_data_vendor_instruments_request::limit},
        utility::codec::field{"order", &list_market_data_vendor_instruments_request::order},
        utility::codec::field{"filter", &list_market_data_vendor_instruments_request::filter},
        utility::codec::field{"as_of", &list_market_data_vendor_instruments_request::as_of},
    };
}

/**
 * @brief The members of list_market_data_vendor_instruments_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_market_data_vendor_instruments_response>) {
    return std::tuple{
        utility::codec::field{"result", &list_market_data_vendor_instruments_response::result},
        utility::codec::field{"vendor_instruments",
                              &list_market_data_vendor_instruments_response::vendor_instruments},
        utility::codec::field{"total", &list_market_data_vendor_instruments_response::total},
    };
}

/**
 * @brief The members of get_market_data_vendor_instrument_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_market_data_vendor_instrument_request>) {
    return std::tuple{
        utility::codec::field{"key", &get_market_data_vendor_instrument_request::key},
    };
}

/**
 * @brief The members of get_market_data_vendor_instrument_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_market_data_vendor_instrument_response>) {
    return std::tuple{
        utility::codec::field{"result", &get_market_data_vendor_instrument_response::result},
        utility::codec::field{
            "market_data_vendor_instrument",
            &get_market_data_vendor_instrument_response::market_data_vendor_instrument},
    };
}

/**
 * @brief The members of get_many_market_data_vendor_instruments_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_many_market_data_vendor_instruments_request>) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_market_data_vendor_instruments_request::keys},
    };
}

/**
 * @brief The members of get_many_market_data_vendor_instruments_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_many_market_data_vendor_instruments_response>) {
    return std::tuple{
        utility::codec::field{"result", &get_many_market_data_vendor_instruments_response::result},
        utility::codec::field{"entries",
                              &get_many_market_data_vendor_instruments_response::entries},
    };
}

/**
 * @brief The members of put_market_data_vendor_instrument_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<put_market_data_vendor_instrument_request>) {
    return std::tuple{
        utility::codec::field{"change", &put_market_data_vendor_instrument_request::change},
        utility::codec::field{"intent", &put_market_data_vendor_instrument_request::intent},
    };
}

/**
 * @brief The members of put_market_data_vendor_instrument_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<put_market_data_vendor_instrument_response>) {
    return std::tuple{
        utility::codec::field{"result", &put_market_data_vendor_instrument_response::result},
        utility::codec::field{
            "market_data_vendor_instrument",
            &put_market_data_vendor_instrument_response::market_data_vendor_instrument},
    };
}

/**
 * @brief The members of put_many_market_data_vendor_instruments_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<put_many_market_data_vendor_instruments_request>) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_market_data_vendor_instruments_request::changes},
        utility::codec::field{"intent", &put_many_market_data_vendor_instruments_request::intent},
    };
}

/**
 * @brief The members of put_many_market_data_vendor_instruments_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<put_many_market_data_vendor_instruments_response>) {
    return std::tuple{
        utility::codec::field{"result", &put_many_market_data_vendor_instruments_response::result},
        utility::codec::field{
            "vendor_instruments",
            &put_many_market_data_vendor_instruments_response::vendor_instruments},
    };
}

/**
 * @brief The members of delete_market_data_vendor_instrument_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_market_data_vendor_instrument_request>) {
    return std::tuple{
        utility::codec::field{"removal", &delete_market_data_vendor_instrument_request::removal},
        utility::codec::field{"intent", &delete_market_data_vendor_instrument_request::intent},
    };
}

/**
 * @brief The members of delete_market_data_vendor_instrument_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_market_data_vendor_instrument_response>) {
    return std::tuple{
        utility::codec::field{"result", &delete_market_data_vendor_instrument_response::result},
    };
}

/**
 * @brief The members of delete_many_market_data_vendor_instruments_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_many_market_data_vendor_instruments_request>) {
    return std::tuple{
        utility::codec::field{"removals",
                              &delete_many_market_data_vendor_instruments_request::removals},
        utility::codec::field{"intent",
                              &delete_many_market_data_vendor_instruments_request::intent},
    };
}

/**
 * @brief The members of delete_many_market_data_vendor_instruments_response, named as the wire
 * names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_many_market_data_vendor_instruments_response>) {
    return std::tuple{
        utility::codec::field{"result",
                              &delete_many_market_data_vendor_instruments_response::result},
    };
}

/**
 * @brief The members of list_market_data_vendor_instrument_versions_request, named as the wire
 * names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_market_data_vendor_instrument_versions_request>) {
    return std::tuple{
        utility::codec::field{"key", &list_market_data_vendor_instrument_versions_request::key},
        utility::codec::field{"offset",
                              &list_market_data_vendor_instrument_versions_request::offset},
        utility::codec::field{"limit", &list_market_data_vendor_instrument_versions_request::limit},
        utility::codec::field{"order", &list_market_data_vendor_instrument_versions_request::order},
        utility::codec::field{"filter",
                              &list_market_data_vendor_instrument_versions_request::filter},
    };
}

/**
 * @brief The members of list_market_data_vendor_instrument_versions_response, named as the wire
 * names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_market_data_vendor_instrument_versions_response>) {
    return std::tuple{
        utility::codec::field{"result",
                              &list_market_data_vendor_instrument_versions_response::result},
        utility::codec::field{"versions",
                              &list_market_data_vendor_instrument_versions_response::versions},
        utility::codec::field{"total",
                              &list_market_data_vendor_instrument_versions_response::total},
    };
}

/**
 * @brief The members of get_market_data_vendor_instrument_version_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_market_data_vendor_instrument_version_request>) {
    return std::tuple{
        utility::codec::field{"key", &get_market_data_vendor_instrument_version_request::key},
    };
}

/**
 * @brief The members of get_market_data_vendor_instrument_version_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_market_data_vendor_instrument_version_response>) {
    return std::tuple{
        utility::codec::field{"result",
                              &get_market_data_vendor_instrument_version_response::result},
        utility::codec::field{"version",
                              &get_market_data_vendor_instrument_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::market_data_vendor_instrument_key> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::market_data_vendor_instrument_key& v);
    static ores::marketdata::messaging::market_data_vendor_instrument_key
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::market_data_vendor_instrument_key& v);
    static ores::marketdata::messaging::market_data_vendor_instrument_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::market_data_vendor_instrument_write> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::market_data_vendor_instrument_write& v);
    static ores::marketdata::messaging::market_data_vendor_instrument_write
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::market_data_vendor_instrument_write& v);
    static ores::marketdata::messaging::market_data_vendor_instrument_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::market_data_vendor_instrument_change> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::market_data_vendor_instrument_change& v);
    static ores::marketdata::messaging::market_data_vendor_instrument_change
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::market_data_vendor_instrument_change& v);
    static ores::marketdata::messaging::market_data_vendor_instrument_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::market_data_vendor_instrument_removal> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::market_data_vendor_instrument_removal& v);
    static ores::marketdata::messaging::market_data_vendor_instrument_removal
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::market_data_vendor_instrument_removal& v);
    static ores::marketdata::messaging::market_data_vendor_instrument_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::market_data_vendor_instrument_lookup> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::market_data_vendor_instrument_lookup& v);
    static ores::marketdata::messaging::market_data_vendor_instrument_lookup
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::market_data_vendor_instrument_lookup& v);
    static ores::marketdata::messaging::market_data_vendor_instrument_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::market_data_vendor_instruments_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::market_data_vendor_instruments_filter& v);
    static ores::marketdata::messaging::market_data_vendor_instruments_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::market_data_vendor_instruments_filter& v);
    static ores::marketdata::messaging::market_data_vendor_instruments_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::market_data_vendor_instrument_event> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::market_data_vendor_instrument_event& v);
    static ores::marketdata::messaging::market_data_vendor_instrument_event
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::market_data_vendor_instrument_event& v);
    static ores::marketdata::messaging::market_data_vendor_instrument_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::market_data_vendor_instrument_version_key> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::market_data_vendor_instrument_version_key& v);
    static ores::marketdata::messaging::market_data_vendor_instrument_version_key
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::market_data_vendor_instrument_version_key& v);
    static ores::marketdata::messaging::market_data_vendor_instrument_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::market_data_vendor_instrument_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::market_data_vendor_instrument_versions_filter& v);
    static ores::marketdata::messaging::market_data_vendor_instrument_versions_filter
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::marketdata::messaging::market_data_vendor_instrument_versions_filter& v);
    static ores::marketdata::messaging::market_data_vendor_instrument_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::list_market_data_vendor_instruments_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::list_market_data_vendor_instruments_request& v);
    static ores::marketdata::messaging::list_market_data_vendor_instruments_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::marketdata::messaging::list_market_data_vendor_instruments_request& v);
    static ores::marketdata::messaging::list_market_data_vendor_instruments_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::list_market_data_vendor_instruments_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::list_market_data_vendor_instruments_response& v);
    static ores::marketdata::messaging::list_market_data_vendor_instruments_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::marketdata::messaging::list_market_data_vendor_instruments_response& v);
    static ores::marketdata::messaging::list_market_data_vendor_instruments_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::get_market_data_vendor_instrument_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::get_market_data_vendor_instrument_request& v);
    static ores::marketdata::messaging::get_market_data_vendor_instrument_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::get_market_data_vendor_instrument_request& v);
    static ores::marketdata::messaging::get_market_data_vendor_instrument_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::get_market_data_vendor_instrument_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::get_market_data_vendor_instrument_response& v);
    static ores::marketdata::messaging::get_market_data_vendor_instrument_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::get_market_data_vendor_instrument_response& v);
    static ores::marketdata::messaging::get_market_data_vendor_instrument_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::get_many_market_data_vendor_instruments_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::marketdata::messaging::get_many_market_data_vendor_instruments_request& v);
    static ores::marketdata::messaging::get_many_market_data_vendor_instruments_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::marketdata::messaging::get_many_market_data_vendor_instruments_request& v);
    static ores::marketdata::messaging::get_many_market_data_vendor_instruments_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::get_many_market_data_vendor_instruments_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::marketdata::messaging::get_many_market_data_vendor_instruments_response& v);
    static ores::marketdata::messaging::get_many_market_data_vendor_instruments_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::marketdata::messaging::get_many_market_data_vendor_instruments_response& v);
    static ores::marketdata::messaging::get_many_market_data_vendor_instruments_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::put_market_data_vendor_instrument_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::put_market_data_vendor_instrument_request& v);
    static ores::marketdata::messaging::put_market_data_vendor_instrument_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::put_market_data_vendor_instrument_request& v);
    static ores::marketdata::messaging::put_market_data_vendor_instrument_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::put_market_data_vendor_instrument_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::put_market_data_vendor_instrument_response& v);
    static ores::marketdata::messaging::put_market_data_vendor_instrument_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::put_market_data_vendor_instrument_response& v);
    static ores::marketdata::messaging::put_market_data_vendor_instrument_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::put_many_market_data_vendor_instruments_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::marketdata::messaging::put_many_market_data_vendor_instruments_request& v);
    static ores::marketdata::messaging::put_many_market_data_vendor_instruments_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::marketdata::messaging::put_many_market_data_vendor_instruments_request& v);
    static ores::marketdata::messaging::put_many_market_data_vendor_instruments_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::put_many_market_data_vendor_instruments_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::marketdata::messaging::put_many_market_data_vendor_instruments_response& v);
    static ores::marketdata::messaging::put_many_market_data_vendor_instruments_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::marketdata::messaging::put_many_market_data_vendor_instruments_response& v);
    static ores::marketdata::messaging::put_many_market_data_vendor_instruments_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::delete_market_data_vendor_instrument_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::delete_market_data_vendor_instrument_request& v);
    static ores::marketdata::messaging::delete_market_data_vendor_instrument_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::marketdata::messaging::delete_market_data_vendor_instrument_request& v);
    static ores::marketdata::messaging::delete_market_data_vendor_instrument_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::delete_market_data_vendor_instrument_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::delete_market_data_vendor_instrument_response& v);
    static ores::marketdata::messaging::delete_market_data_vendor_instrument_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::marketdata::messaging::delete_market_data_vendor_instrument_response& v);
    static ores::marketdata::messaging::delete_market_data_vendor_instrument_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::delete_many_market_data_vendor_instruments_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::marketdata::messaging::delete_many_market_data_vendor_instruments_request& v);
    static ores::marketdata::messaging::delete_many_market_data_vendor_instruments_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::marketdata::messaging::delete_many_market_data_vendor_instruments_request& v);
    static ores::marketdata::messaging::delete_many_market_data_vendor_instruments_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::delete_many_market_data_vendor_instruments_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::marketdata::messaging::delete_many_market_data_vendor_instruments_response& v);
    static ores::marketdata::messaging::delete_many_market_data_vendor_instruments_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::marketdata::messaging::delete_many_market_data_vendor_instruments_response& v);
    static ores::marketdata::messaging::delete_many_market_data_vendor_instruments_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::list_market_data_vendor_instrument_versions_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::marketdata::messaging::list_market_data_vendor_instrument_versions_request& v);
    static ores::marketdata::messaging::list_market_data_vendor_instrument_versions_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::marketdata::messaging::list_market_data_vendor_instrument_versions_request& v);
    static ores::marketdata::messaging::list_market_data_vendor_instrument_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::list_market_data_vendor_instrument_versions_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::marketdata::messaging::list_market_data_vendor_instrument_versions_response& v);
    static ores::marketdata::messaging::list_market_data_vendor_instrument_versions_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::marketdata::messaging::list_market_data_vendor_instrument_versions_response& v);
    static ores::marketdata::messaging::list_market_data_vendor_instrument_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::get_market_data_vendor_instrument_version_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::marketdata::messaging::get_market_data_vendor_instrument_version_request& v);
    static ores::marketdata::messaging::get_market_data_vendor_instrument_version_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::marketdata::messaging::get_market_data_vendor_instrument_version_request& v);
    static ores::marketdata::messaging::get_market_data_vendor_instrument_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::get_market_data_vendor_instrument_version_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::marketdata::messaging::get_market_data_vendor_instrument_version_response& v);
    static ores::marketdata::messaging::get_market_data_vendor_instrument_version_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::marketdata::messaging::get_market_data_vendor_instrument_version_response& v);
    static ores::marketdata::messaging::get_market_data_vendor_instrument_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
