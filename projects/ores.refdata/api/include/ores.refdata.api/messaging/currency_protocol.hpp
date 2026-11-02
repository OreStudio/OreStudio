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
#ifndef ORES_REFDATA_API_MESSAGING_CURRENCY_PROTOCOL_HPP
#define ORES_REFDATA_API_MESSAGING_CURRENCY_PROTOCOL_HPP

#include "ores.refdata.api/domain/currency.hpp"
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

struct currency_key {
    std::string iso_code;

    friend bool operator==(const currency_key&, const currency_key&) = default;
};

struct currency_write {
    std::string iso_code;
    std::string name;
    std::string numeric_code;
    std::string symbol;
    std::string fraction_symbol;
    int fractions_per_unit;
    std::string rounding_type;
    int rounding_precision;
    std::string format;
    std::string monetary_nature;
    std::string market_tier;
    std::optional<std::string> ore_currency_type;
    std::optional<boost::uuids::uuid> image_id;
    int spot_days;
    std::string day_basis;
    int base_precedence;

    friend bool operator==(const currency_write&, const currency_write&) = default;
};

struct currency_change {
    currency_write write;
    ores::utility::domain::precondition precondition;

    friend bool operator==(const currency_change&, const currency_change&) = default;
};

struct currency_removal {
    currency_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;

    friend bool operator==(const currency_removal&, const currency_removal&) = default;
};

struct currency_lookup {
    currency_key key;
    std::optional<ores::refdata::domain::currency> currency;

    friend bool operator==(const currency_lookup&, const currency_lookup&) = default;
};

struct currencies_filter {
    std::optional<std::vector<std::string>> iso_code_one_of;
    std::optional<std::string> search;

    friend bool operator==(const currencies_filter&, const currencies_filter&) = default;
};

struct currency_event {
    boost::uuids::uuid event_id;
    currency_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;

    friend bool operator==(const currency_event&, const currency_event&) = default;
};

struct currency_version_key {
    currency_key currency;
    std::uint32_t version;

    friend bool operator==(const currency_version_key&, const currency_version_key&) = default;
};

struct currency_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;

    friend bool operator==(const currency_versions_filter&,
                           const currency_versions_filter&) = default;
};

struct list_currencies_request {
    using response_type = struct list_currencies_response;
    static constexpr std::string_view nats_subject = "refdata.v1.currencies.list";
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
    std::optional<currencies_filter> filter;
    std::optional<std::string> as_of;

    friend bool operator==(const list_currencies_request&,
                           const list_currencies_request&) = default;
};

struct list_currencies_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::currency> currencies;
    std::uint64_t total;

    friend bool operator==(const list_currencies_response&,
                           const list_currencies_response&) = default;
};

struct get_currency_request {
    using response_type = struct get_currency_response;
    static constexpr std::string_view nats_subject = "refdata.v1.currencies.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    currency_key key;

    friend bool operator==(const get_currency_request&, const get_currency_request&) = default;
};

struct get_currency_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::currency> currency;

    friend bool operator==(const get_currency_response&, const get_currency_response&) = default;
};

struct get_many_currencies_request {
    using response_type = struct get_many_currencies_response;
    static constexpr std::string_view nats_subject = "refdata.v1.currencies.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<currency_key> keys;

    friend bool operator==(const get_many_currencies_request&,
                           const get_many_currencies_request&) = default;
};

struct get_many_currencies_response {
    ores::utility::domain::result result;
    std::vector<currency_lookup> entries;

    friend bool operator==(const get_many_currencies_response&,
                           const get_many_currencies_response&) = default;
};

struct put_currency_request {
    using response_type = struct put_currency_response;
    static constexpr std::string_view nats_subject = "refdata.v1.currencies.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    currency_change change;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_currency_request&, const put_currency_request&) = default;
};

struct put_currency_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::currency> currency;

    friend bool operator==(const put_currency_response&, const put_currency_response&) = default;
};

struct put_many_currencies_request {
    using response_type = struct put_many_currencies_response;
    static constexpr std::string_view nats_subject = "refdata.v1.currencies.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<currency_change> changes;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_many_currencies_request&,
                           const put_many_currencies_request&) = default;
};

struct put_many_currencies_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::currency> currencies;

    friend bool operator==(const put_many_currencies_response&,
                           const put_many_currencies_response&) = default;
};

struct delete_currency_request {
    using response_type = struct delete_currency_response;
    static constexpr std::string_view nats_subject = "refdata.v1.currencies.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    currency_removal removal;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_currency_request&,
                           const delete_currency_request&) = default;
};

struct delete_currency_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_currency_response&,
                           const delete_currency_response&) = default;
};

struct delete_many_currencies_request {
    using response_type = struct delete_many_currencies_response;
    static constexpr std::string_view nats_subject = "refdata.v1.currencies.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<currency_removal> removals;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_many_currencies_request&,
                           const delete_many_currencies_request&) = default;
};

struct delete_many_currencies_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_many_currencies_response&,
                           const delete_many_currencies_response&) = default;
};

struct list_currency_versions_request {
    using response_type = struct list_currency_versions_response;
    static constexpr std::string_view nats_subject = "refdata.v1.currencies_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    currency_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<currency_versions_filter> filter;

    friend bool operator==(const list_currency_versions_request&,
                           const list_currency_versions_request&) = default;
};

struct list_currency_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::currency> versions;
    std::uint64_t total;

    friend bool operator==(const list_currency_versions_response&,
                           const list_currency_versions_response&) = default;
};

struct get_currency_version_request {
    using response_type = struct get_currency_version_response;
    static constexpr std::string_view nats_subject = "refdata.v1.currencies_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    currency_version_key key;

    friend bool operator==(const get_currency_version_request&,
                           const get_currency_version_request&) = default;
};

struct get_currency_version_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::currency> version;

    friend bool operator==(const get_currency_version_response&,
                           const get_currency_version_response&) = default;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace currency_event_subjects {
inline constexpr std::string_view created = "refdata.v1.currencies_events.created";
inline constexpr std::string_view updated = "refdata.v1.currencies_events.updated";
inline constexpr std::string_view deleted = "refdata.v1.currencies_events.deleted";
}
/**
 * @brief The members of currency_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const currency_key&) {
    return std::tuple{
        utility::codec::field{"iso_code", &currency_key::iso_code},
    };
}

/**
 * @brief The members of currency_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const currency_write&) {
    return std::tuple{
        utility::codec::field{"iso_code", &currency_write::iso_code},
        utility::codec::field{"name", &currency_write::name},
        utility::codec::field{"numeric_code", &currency_write::numeric_code},
        utility::codec::field{"symbol", &currency_write::symbol},
        utility::codec::field{"fraction_symbol", &currency_write::fraction_symbol},
        utility::codec::field{"fractions_per_unit", &currency_write::fractions_per_unit},
        utility::codec::field{"rounding_type", &currency_write::rounding_type},
        utility::codec::field{"rounding_precision", &currency_write::rounding_precision},
        utility::codec::field{"format", &currency_write::format},
        utility::codec::field{"monetary_nature", &currency_write::monetary_nature},
        utility::codec::field{"market_tier", &currency_write::market_tier},
        utility::codec::field{"ore_currency_type", &currency_write::ore_currency_type},
        utility::codec::field{"image_id", &currency_write::image_id},
        utility::codec::field{"spot_days", &currency_write::spot_days},
        utility::codec::field{"day_basis", &currency_write::day_basis},
        utility::codec::field{"base_precedence", &currency_write::base_precedence},
    };
}

/**
 * @brief The members of currency_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const currency_change&) {
    return std::tuple{
        utility::codec::field{"write", &currency_change::write},
        utility::codec::field{"precondition", &currency_change::precondition},
    };
}

/**
 * @brief The members of currency_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const currency_removal&) {
    return std::tuple{
        utility::codec::field{"key", &currency_removal::key},
        utility::codec::field{"precondition", &currency_removal::precondition},
    };
}

/**
 * @brief The members of currency_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const currency_lookup&) {
    return std::tuple{
        utility::codec::field{"key", &currency_lookup::key},
        utility::codec::field{"currency", &currency_lookup::currency},
    };
}

/**
 * @brief The members of currencies_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const currencies_filter&) {
    return std::tuple{
        utility::codec::field{"iso_code_one_of", &currencies_filter::iso_code_one_of},
        utility::codec::field{"search", &currencies_filter::search},
    };
}

/**
 * @brief The members of currency_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const currency_event&) {
    return std::tuple{
        utility::codec::field{"event_id", &currency_event::event_id},
        utility::codec::field{"key", &currency_event::key},
        utility::codec::field{"action", &currency_event::action},
        utility::codec::field{"version", &currency_event::version},
        utility::codec::field{"occurred_at", &currency_event::occurred_at},
        utility::codec::field{"correlation_id", &currency_event::correlation_id},
    };
}

/**
 * @brief The members of currency_version_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const currency_version_key&) {
    return std::tuple{
        utility::codec::field{"currency", &currency_version_key::currency},
        utility::codec::field{"version", &currency_version_key::version},
    };
}

/**
 * @brief The members of currency_versions_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const currency_versions_filter&) {
    return std::tuple{
        utility::codec::field{"version", &currency_versions_filter::version},
        utility::codec::field{"from_version", &currency_versions_filter::from_version},
        utility::codec::field{"to_version", &currency_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_currencies_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_currencies_request&) {
    return std::tuple{
        utility::codec::field{"offset", &list_currencies_request::offset},
        utility::codec::field{"limit", &list_currencies_request::limit},
        utility::codec::field{"order", &list_currencies_request::order},
        utility::codec::field{"filter", &list_currencies_request::filter},
        utility::codec::field{"as_of", &list_currencies_request::as_of},
    };
}

/**
 * @brief The members of list_currencies_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_currencies_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_currencies_response::result},
        utility::codec::field{"currencies", &list_currencies_response::currencies},
        utility::codec::field{"total", &list_currencies_response::total},
    };
}

/**
 * @brief The members of get_currency_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_currency_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_currency_request::key},
    };
}

/**
 * @brief The members of get_currency_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_currency_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_currency_response::result},
        utility::codec::field{"currency", &get_currency_response::currency},
    };
}

/**
 * @brief The members of get_many_currencies_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_currencies_request&) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_currencies_request::keys},
    };
}

/**
 * @brief The members of get_many_currencies_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_currencies_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_many_currencies_response::result},
        utility::codec::field{"entries", &get_many_currencies_response::entries},
    };
}

/**
 * @brief The members of put_currency_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_currency_request&) {
    return std::tuple{
        utility::codec::field{"change", &put_currency_request::change},
        utility::codec::field{"intent", &put_currency_request::intent},
    };
}

/**
 * @brief The members of put_currency_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_currency_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_currency_response::result},
        utility::codec::field{"currency", &put_currency_response::currency},
    };
}

/**
 * @brief The members of put_many_currencies_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_currencies_request&) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_currencies_request::changes},
        utility::codec::field{"intent", &put_many_currencies_request::intent},
    };
}

/**
 * @brief The members of put_many_currencies_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_currencies_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_many_currencies_response::result},
        utility::codec::field{"currencies", &put_many_currencies_response::currencies},
    };
}

/**
 * @brief The members of delete_currency_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_currency_request&) {
    return std::tuple{
        utility::codec::field{"removal", &delete_currency_request::removal},
        utility::codec::field{"intent", &delete_currency_request::intent},
    };
}

/**
 * @brief The members of delete_currency_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_currency_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_currency_response::result},
    };
}

/**
 * @brief The members of delete_many_currencies_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_currencies_request&) {
    return std::tuple{
        utility::codec::field{"removals", &delete_many_currencies_request::removals},
        utility::codec::field{"intent", &delete_many_currencies_request::intent},
    };
}

/**
 * @brief The members of delete_many_currencies_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_currencies_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_currencies_response::result},
    };
}

/**
 * @brief The members of list_currency_versions_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_currency_versions_request&) {
    return std::tuple{
        utility::codec::field{"key", &list_currency_versions_request::key},
        utility::codec::field{"offset", &list_currency_versions_request::offset},
        utility::codec::field{"limit", &list_currency_versions_request::limit},
        utility::codec::field{"order", &list_currency_versions_request::order},
        utility::codec::field{"filter", &list_currency_versions_request::filter},
    };
}

/**
 * @brief The members of list_currency_versions_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_currency_versions_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_currency_versions_response::result},
        utility::codec::field{"versions", &list_currency_versions_response::versions},
        utility::codec::field{"total", &list_currency_versions_response::total},
    };
}

/**
 * @brief The members of get_currency_version_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_currency_version_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_currency_version_request::key},
    };
}

/**
 * @brief The members of get_currency_version_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_currency_version_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_currency_version_response::result},
        utility::codec::field{"version", &get_currency_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::currency_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::currency_key& v);
    static ores::refdata::messaging::currency_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::currency_key& v);
    static ores::refdata::messaging::currency_key read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::currency_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::currency_write& v);
    static ores::refdata::messaging::currency_write read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::currency_write& v);
    static ores::refdata::messaging::currency_write read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::currency_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::currency_change& v);
    static ores::refdata::messaging::currency_change read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::currency_change& v);
    static ores::refdata::messaging::currency_change read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::currency_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::currency_removal& v);
    static ores::refdata::messaging::currency_removal read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::currency_removal& v);
    static ores::refdata::messaging::currency_removal read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::currency_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::currency_lookup& v);
    static ores::refdata::messaging::currency_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::currency_lookup& v);
    static ores::refdata::messaging::currency_lookup read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::currencies_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::currencies_filter& v);
    static ores::refdata::messaging::currencies_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::currencies_filter& v);
    static ores::refdata::messaging::currencies_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::currency_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::currency_event& v);
    static ores::refdata::messaging::currency_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::currency_event& v);
    static ores::refdata::messaging::currency_event read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::currency_version_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::currency_version_key& v);
    static ores::refdata::messaging::currency_version_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::currency_version_key& v);
    static ores::refdata::messaging::currency_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::currency_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::currency_versions_filter& v);
    static ores::refdata::messaging::currency_versions_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::currency_versions_filter& v);
    static ores::refdata::messaging::currency_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::list_currencies_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::list_currencies_request& v);
    static ores::refdata::messaging::list_currencies_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::list_currencies_request& v);
    static ores::refdata::messaging::list_currencies_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::list_currencies_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::list_currencies_response& v);
    static ores::refdata::messaging::list_currencies_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::list_currencies_response& v);
    static ores::refdata::messaging::list_currencies_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::get_currency_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::get_currency_request& v);
    static ores::refdata::messaging::get_currency_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::get_currency_request& v);
    static ores::refdata::messaging::get_currency_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::get_currency_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::get_currency_response& v);
    static ores::refdata::messaging::get_currency_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::get_currency_response& v);
    static ores::refdata::messaging::get_currency_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::get_many_currencies_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::get_many_currencies_request& v);
    static ores::refdata::messaging::get_many_currencies_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_many_currencies_request& v);
    static ores::refdata::messaging::get_many_currencies_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::get_many_currencies_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::get_many_currencies_response& v);
    static ores::refdata::messaging::get_many_currencies_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_many_currencies_response& v);
    static ores::refdata::messaging::get_many_currencies_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::put_currency_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::put_currency_request& v);
    static ores::refdata::messaging::put_currency_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::put_currency_request& v);
    static ores::refdata::messaging::put_currency_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::put_currency_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::put_currency_response& v);
    static ores::refdata::messaging::put_currency_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::put_currency_response& v);
    static ores::refdata::messaging::put_currency_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::put_many_currencies_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::put_many_currencies_request& v);
    static ores::refdata::messaging::put_many_currencies_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_many_currencies_request& v);
    static ores::refdata::messaging::put_many_currencies_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::put_many_currencies_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::put_many_currencies_response& v);
    static ores::refdata::messaging::put_many_currencies_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_many_currencies_response& v);
    static ores::refdata::messaging::put_many_currencies_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::delete_currency_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::delete_currency_request& v);
    static ores::refdata::messaging::delete_currency_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::delete_currency_request& v);
    static ores::refdata::messaging::delete_currency_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::delete_currency_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::delete_currency_response& v);
    static ores::refdata::messaging::delete_currency_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::delete_currency_response& v);
    static ores::refdata::messaging::delete_currency_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_many_currencies_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_many_currencies_request& v);
    static ores::refdata::messaging::delete_many_currencies_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_many_currencies_request& v);
    static ores::refdata::messaging::delete_many_currencies_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_many_currencies_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_many_currencies_response& v);
    static ores::refdata::messaging::delete_many_currencies_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_many_currencies_response& v);
    static ores::refdata::messaging::delete_many_currencies_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_currency_versions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_currency_versions_request& v);
    static ores::refdata::messaging::list_currency_versions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_currency_versions_request& v);
    static ores::refdata::messaging::list_currency_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_currency_versions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_currency_versions_response& v);
    static ores::refdata::messaging::list_currency_versions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_currency_versions_response& v);
    static ores::refdata::messaging::list_currency_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::get_currency_version_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::get_currency_version_request& v);
    static ores::refdata::messaging::get_currency_version_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_currency_version_request& v);
    static ores::refdata::messaging::get_currency_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_currency_version_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::get_currency_version_response& v);
    static ores::refdata::messaging::get_currency_version_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_currency_version_response& v);
    static ores::refdata::messaging::get_currency_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
