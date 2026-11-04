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
#ifndef ORES_MARKETDATA_API_MESSAGING_MARKET_SERIES_ASSET_CLASS_PROTOCOL_HPP
#define ORES_MARKETDATA_API_MESSAGING_MARKET_SERIES_ASSET_CLASS_PROTOCOL_HPP

#include "ores.marketdata.api/domain/market_series_asset_class.hpp"
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

struct market_series_asset_class_key {
    boost::uuids::uuid market_series_id;
    std::string asset_class_code;

    friend bool operator==(const market_series_asset_class_key&,
                           const market_series_asset_class_key&) = default;
};

struct market_series_asset_class_write {
    boost::uuids::uuid market_series_id;
    std::string asset_class_code;

    friend bool operator==(const market_series_asset_class_write&,
                           const market_series_asset_class_write&) = default;
};

struct market_series_asset_class_change {
    market_series_asset_class_write write;
    ores::utility::domain::precondition precondition;

    friend bool operator==(const market_series_asset_class_change&,
                           const market_series_asset_class_change&) = default;
};

struct market_series_asset_class_removal {
    market_series_asset_class_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;

    friend bool operator==(const market_series_asset_class_removal&,
                           const market_series_asset_class_removal&) = default;
};

struct market_series_asset_class_lookup {
    market_series_asset_class_key key;
    std::optional<ores::marketdata::domain::market_series_asset_class> market_series_asset_class;

    friend bool operator==(const market_series_asset_class_lookup&,
                           const market_series_asset_class_lookup&) = default;
};

struct market_series_asset_classes_filter {
    std::optional<boost::uuids::uuid> market_series_id;
    std::optional<std::vector<boost::uuids::uuid>> market_series_id_one_of;

    friend bool operator==(const market_series_asset_classes_filter&,
                           const market_series_asset_classes_filter&) = default;
};

struct list_market_series_asset_classes_request {
    using response_type = struct list_market_series_asset_classes_response;
    static constexpr std::string_view nats_subject =
        "marketdata.v1.market_series_asset_classes.list";
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
    std::optional<market_series_asset_classes_filter> filter;

    friend bool operator==(const list_market_series_asset_classes_request&,
                           const list_market_series_asset_classes_request&) = default;
};

struct list_market_series_asset_classes_response {
    ores::utility::domain::result result;
    std::vector<ores::marketdata::domain::market_series_asset_class> market_series_asset_classes;
    std::uint64_t total;

    friend bool operator==(const list_market_series_asset_classes_response&,
                           const list_market_series_asset_classes_response&) = default;
};

struct get_market_series_asset_class_request {
    using response_type = struct get_market_series_asset_class_response;
    static constexpr std::string_view nats_subject =
        "marketdata.v1.market_series_asset_classes.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    market_series_asset_class_key key;

    friend bool operator==(const get_market_series_asset_class_request&,
                           const get_market_series_asset_class_request&) = default;
};

struct get_market_series_asset_class_response {
    ores::utility::domain::result result;
    std::optional<ores::marketdata::domain::market_series_asset_class> market_series_asset_class;

    friend bool operator==(const get_market_series_asset_class_response&,
                           const get_market_series_asset_class_response&) = default;
};

struct get_many_market_series_asset_classes_request {
    using response_type = struct get_many_market_series_asset_classes_response;
    static constexpr std::string_view nats_subject =
        "marketdata.v1.market_series_asset_classes.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<market_series_asset_class_key> keys;

    friend bool operator==(const get_many_market_series_asset_classes_request&,
                           const get_many_market_series_asset_classes_request&) = default;
};

struct get_many_market_series_asset_classes_response {
    ores::utility::domain::result result;
    std::vector<market_series_asset_class_lookup> entries;

    friend bool operator==(const get_many_market_series_asset_classes_response&,
                           const get_many_market_series_asset_classes_response&) = default;
};

struct put_market_series_asset_class_request {
    using response_type = struct put_market_series_asset_class_response;
    static constexpr std::string_view nats_subject =
        "marketdata.v1.market_series_asset_classes.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    market_series_asset_class_change change;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_market_series_asset_class_request&,
                           const put_market_series_asset_class_request&) = default;
};

struct put_market_series_asset_class_response {
    ores::utility::domain::result result;
    std::optional<ores::marketdata::domain::market_series_asset_class> market_series_asset_class;

    friend bool operator==(const put_market_series_asset_class_response&,
                           const put_market_series_asset_class_response&) = default;
};

struct put_many_market_series_asset_classes_request {
    using response_type = struct put_many_market_series_asset_classes_response;
    static constexpr std::string_view nats_subject =
        "marketdata.v1.market_series_asset_classes.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<market_series_asset_class_change> changes;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_many_market_series_asset_classes_request&,
                           const put_many_market_series_asset_classes_request&) = default;
};

struct put_many_market_series_asset_classes_response {
    ores::utility::domain::result result;
    std::vector<ores::marketdata::domain::market_series_asset_class> market_series_asset_classes;

    friend bool operator==(const put_many_market_series_asset_classes_response&,
                           const put_many_market_series_asset_classes_response&) = default;
};

struct delete_market_series_asset_class_request {
    using response_type = struct delete_market_series_asset_class_response;
    static constexpr std::string_view nats_subject =
        "marketdata.v1.market_series_asset_classes.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    market_series_asset_class_removal removal;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_market_series_asset_class_request&,
                           const delete_market_series_asset_class_request&) = default;
};

struct delete_market_series_asset_class_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_market_series_asset_class_response&,
                           const delete_market_series_asset_class_response&) = default;
};

struct delete_many_market_series_asset_classes_request {
    using response_type = struct delete_many_market_series_asset_classes_response;
    static constexpr std::string_view nats_subject =
        "marketdata.v1.market_series_asset_classes.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<market_series_asset_class_removal> removals;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_many_market_series_asset_classes_request&,
                           const delete_many_market_series_asset_classes_request&) = default;
};

struct delete_many_market_series_asset_classes_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_many_market_series_asset_classes_response&,
                           const delete_many_market_series_asset_classes_response&) = default;
};

struct list_by_market_series_id_market_series_asset_classes_request {
    using response_type = struct list_by_market_series_id_market_series_asset_classes_response;
    static constexpr std::string_view nats_subject =
        "marketdata.v1.market_series_asset_classes.list_by_market_series_id";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    boost::uuids::uuid market_series_id;
    ores::utility::domain::scope scope = ores::utility::domain::scope::direct;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<market_series_asset_classes_filter> filter;

    friend bool
    operator==(const list_by_market_series_id_market_series_asset_classes_request&,
               const list_by_market_series_id_market_series_asset_classes_request&) = default;
};

struct list_by_market_series_id_market_series_asset_classes_response {
    ores::utility::domain::result result;
    std::vector<ores::marketdata::domain::market_series_asset_class> market_series_asset_classes;
    std::uint64_t total;

    friend bool
    operator==(const list_by_market_series_id_market_series_asset_classes_response&,
               const list_by_market_series_id_market_series_asset_classes_response&) = default;
};

/**
 * @brief The members of market_series_asset_class_key, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<market_series_asset_class_key>) {
    return std::tuple{
        utility::codec::field{"market_series_id", &market_series_asset_class_key::market_series_id},
        utility::codec::field{"asset_class_code", &market_series_asset_class_key::asset_class_code},
    };
}

/**
 * @brief The members of market_series_asset_class_write, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<market_series_asset_class_write>) {
    return std::tuple{
        utility::codec::field{"market_series_id",
                              &market_series_asset_class_write::market_series_id},
        utility::codec::field{"asset_class_code",
                              &market_series_asset_class_write::asset_class_code},
    };
}

/**
 * @brief The members of market_series_asset_class_change, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<market_series_asset_class_change>) {
    return std::tuple{
        utility::codec::field{"write", &market_series_asset_class_change::write},
        utility::codec::field{"precondition", &market_series_asset_class_change::precondition},
    };
}

/**
 * @brief The members of market_series_asset_class_removal, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<market_series_asset_class_removal>) {
    return std::tuple{
        utility::codec::field{"key", &market_series_asset_class_removal::key},
        utility::codec::field{"precondition", &market_series_asset_class_removal::precondition},
    };
}

/**
 * @brief The members of market_series_asset_class_lookup, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<market_series_asset_class_lookup>) {
    return std::tuple{
        utility::codec::field{"key", &market_series_asset_class_lookup::key},
        utility::codec::field{"market_series_asset_class",
                              &market_series_asset_class_lookup::market_series_asset_class},
    };
}

/**
 * @brief The members of market_series_asset_classes_filter, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<market_series_asset_classes_filter>) {
    return std::tuple{
        utility::codec::field{"market_series_id",
                              &market_series_asset_classes_filter::market_series_id},
        utility::codec::field{"market_series_id_one_of",
                              &market_series_asset_classes_filter::market_series_id_one_of},
    };
}

/**
 * @brief The members of list_market_series_asset_classes_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_market_series_asset_classes_request>) {
    return std::tuple{
        utility::codec::field{"offset", &list_market_series_asset_classes_request::offset},
        utility::codec::field{"limit", &list_market_series_asset_classes_request::limit},
        utility::codec::field{"order", &list_market_series_asset_classes_request::order},
        utility::codec::field{"filter", &list_market_series_asset_classes_request::filter},
    };
}

/**
 * @brief The members of list_market_series_asset_classes_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_market_series_asset_classes_response>) {
    return std::tuple{
        utility::codec::field{"result", &list_market_series_asset_classes_response::result},
        utility::codec::field{
            "market_series_asset_classes",
            &list_market_series_asset_classes_response::market_series_asset_classes},
        utility::codec::field{"total", &list_market_series_asset_classes_response::total},
    };
}

/**
 * @brief The members of get_market_series_asset_class_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_market_series_asset_class_request>) {
    return std::tuple{
        utility::codec::field{"key", &get_market_series_asset_class_request::key},
    };
}

/**
 * @brief The members of get_market_series_asset_class_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_market_series_asset_class_response>) {
    return std::tuple{
        utility::codec::field{"result", &get_market_series_asset_class_response::result},
        utility::codec::field{"market_series_asset_class",
                              &get_market_series_asset_class_response::market_series_asset_class},
    };
}

/**
 * @brief The members of get_many_market_series_asset_classes_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_many_market_series_asset_classes_request>) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_market_series_asset_classes_request::keys},
    };
}

/**
 * @brief The members of get_many_market_series_asset_classes_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_many_market_series_asset_classes_response>) {
    return std::tuple{
        utility::codec::field{"result", &get_many_market_series_asset_classes_response::result},
        utility::codec::field{"entries", &get_many_market_series_asset_classes_response::entries},
    };
}

/**
 * @brief The members of put_market_series_asset_class_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<put_market_series_asset_class_request>) {
    return std::tuple{
        utility::codec::field{"change", &put_market_series_asset_class_request::change},
        utility::codec::field{"intent", &put_market_series_asset_class_request::intent},
    };
}

/**
 * @brief The members of put_market_series_asset_class_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<put_market_series_asset_class_response>) {
    return std::tuple{
        utility::codec::field{"result", &put_market_series_asset_class_response::result},
        utility::codec::field{"market_series_asset_class",
                              &put_market_series_asset_class_response::market_series_asset_class},
    };
}

/**
 * @brief The members of put_many_market_series_asset_classes_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<put_many_market_series_asset_classes_request>) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_market_series_asset_classes_request::changes},
        utility::codec::field{"intent", &put_many_market_series_asset_classes_request::intent},
    };
}

/**
 * @brief The members of put_many_market_series_asset_classes_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<put_many_market_series_asset_classes_response>) {
    return std::tuple{
        utility::codec::field{"result", &put_many_market_series_asset_classes_response::result},
        utility::codec::field{
            "market_series_asset_classes",
            &put_many_market_series_asset_classes_response::market_series_asset_classes},
    };
}

/**
 * @brief The members of delete_market_series_asset_class_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_market_series_asset_class_request>) {
    return std::tuple{
        utility::codec::field{"removal", &delete_market_series_asset_class_request::removal},
        utility::codec::field{"intent", &delete_market_series_asset_class_request::intent},
    };
}

/**
 * @brief The members of delete_market_series_asset_class_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_market_series_asset_class_response>) {
    return std::tuple{
        utility::codec::field{"result", &delete_market_series_asset_class_response::result},
    };
}

/**
 * @brief The members of delete_many_market_series_asset_classes_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_many_market_series_asset_classes_request>) {
    return std::tuple{
        utility::codec::field{"removals",
                              &delete_many_market_series_asset_classes_request::removals},
        utility::codec::field{"intent", &delete_many_market_series_asset_classes_request::intent},
    };
}

/**
 * @brief The members of delete_many_market_series_asset_classes_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_many_market_series_asset_classes_response>) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_market_series_asset_classes_response::result},
    };
}

/**
 * @brief The members of list_by_market_series_id_market_series_asset_classes_request, named as the
 * wire names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_by_market_series_id_market_series_asset_classes_request>) {
    return std::tuple{
        utility::codec::field{
            "market_series_id",
            &list_by_market_series_id_market_series_asset_classes_request::market_series_id},
        utility::codec::field{"scope",
                              &list_by_market_series_id_market_series_asset_classes_request::scope},
        utility::codec::field{
            "offset", &list_by_market_series_id_market_series_asset_classes_request::offset},
        utility::codec::field{"limit",
                              &list_by_market_series_id_market_series_asset_classes_request::limit},
        utility::codec::field{"order",
                              &list_by_market_series_id_market_series_asset_classes_request::order},
        utility::codec::field{
            "filter", &list_by_market_series_id_market_series_asset_classes_request::filter},
    };
}

/**
 * @brief The members of list_by_market_series_id_market_series_asset_classes_response, named as the
 * wire names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_by_market_series_id_market_series_asset_classes_response>) {
    return std::tuple{
        utility::codec::field{
            "result", &list_by_market_series_id_market_series_asset_classes_response::result},
        utility::codec::field{"market_series_asset_classes",
                              &list_by_market_series_id_market_series_asset_classes_response::
                                  market_series_asset_classes},
        utility::codec::field{
            "total", &list_by_market_series_id_market_series_asset_classes_response::total},
    };
}

}
namespace ores::utility::codec {

template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::market_series_asset_class_key> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::market_series_asset_class_key& v);
    static ores::marketdata::messaging::market_series_asset_class_key
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::market_series_asset_class_key& v);
    static ores::marketdata::messaging::market_series_asset_class_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::market_series_asset_class_write> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::market_series_asset_class_write& v);
    static ores::marketdata::messaging::market_series_asset_class_write
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::market_series_asset_class_write& v);
    static ores::marketdata::messaging::market_series_asset_class_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::market_series_asset_class_change> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::market_series_asset_class_change& v);
    static ores::marketdata::messaging::market_series_asset_class_change
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::market_series_asset_class_change& v);
    static ores::marketdata::messaging::market_series_asset_class_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::market_series_asset_class_removal> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::market_series_asset_class_removal& v);
    static ores::marketdata::messaging::market_series_asset_class_removal
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::market_series_asset_class_removal& v);
    static ores::marketdata::messaging::market_series_asset_class_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::market_series_asset_class_lookup> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::market_series_asset_class_lookup& v);
    static ores::marketdata::messaging::market_series_asset_class_lookup
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::market_series_asset_class_lookup& v);
    static ores::marketdata::messaging::market_series_asset_class_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::market_series_asset_classes_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::market_series_asset_classes_filter& v);
    static ores::marketdata::messaging::market_series_asset_classes_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::market_series_asset_classes_filter& v);
    static ores::marketdata::messaging::market_series_asset_classes_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::list_market_series_asset_classes_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::list_market_series_asset_classes_request& v);
    static ores::marketdata::messaging::list_market_series_asset_classes_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::list_market_series_asset_classes_request& v);
    static ores::marketdata::messaging::list_market_series_asset_classes_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::list_market_series_asset_classes_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::list_market_series_asset_classes_response& v);
    static ores::marketdata::messaging::list_market_series_asset_classes_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::list_market_series_asset_classes_response& v);
    static ores::marketdata::messaging::list_market_series_asset_classes_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::get_market_series_asset_class_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::get_market_series_asset_class_request& v);
    static ores::marketdata::messaging::get_market_series_asset_class_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::get_market_series_asset_class_request& v);
    static ores::marketdata::messaging::get_market_series_asset_class_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::get_market_series_asset_class_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::get_market_series_asset_class_response& v);
    static ores::marketdata::messaging::get_market_series_asset_class_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::get_market_series_asset_class_response& v);
    static ores::marketdata::messaging::get_market_series_asset_class_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::get_many_market_series_asset_classes_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::get_many_market_series_asset_classes_request& v);
    static ores::marketdata::messaging::get_many_market_series_asset_classes_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::marketdata::messaging::get_many_market_series_asset_classes_request& v);
    static ores::marketdata::messaging::get_many_market_series_asset_classes_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::get_many_market_series_asset_classes_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::get_many_market_series_asset_classes_response& v);
    static ores::marketdata::messaging::get_many_market_series_asset_classes_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::marketdata::messaging::get_many_market_series_asset_classes_response& v);
    static ores::marketdata::messaging::get_many_market_series_asset_classes_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::put_market_series_asset_class_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::put_market_series_asset_class_request& v);
    static ores::marketdata::messaging::put_market_series_asset_class_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::put_market_series_asset_class_request& v);
    static ores::marketdata::messaging::put_market_series_asset_class_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::put_market_series_asset_class_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::put_market_series_asset_class_response& v);
    static ores::marketdata::messaging::put_market_series_asset_class_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::put_market_series_asset_class_response& v);
    static ores::marketdata::messaging::put_market_series_asset_class_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::put_many_market_series_asset_classes_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::put_many_market_series_asset_classes_request& v);
    static ores::marketdata::messaging::put_many_market_series_asset_classes_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::marketdata::messaging::put_many_market_series_asset_classes_request& v);
    static ores::marketdata::messaging::put_many_market_series_asset_classes_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::put_many_market_series_asset_classes_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::put_many_market_series_asset_classes_response& v);
    static ores::marketdata::messaging::put_many_market_series_asset_classes_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::marketdata::messaging::put_many_market_series_asset_classes_response& v);
    static ores::marketdata::messaging::put_many_market_series_asset_classes_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::delete_market_series_asset_class_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::delete_market_series_asset_class_request& v);
    static ores::marketdata::messaging::delete_market_series_asset_class_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::delete_market_series_asset_class_request& v);
    static ores::marketdata::messaging::delete_market_series_asset_class_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::delete_market_series_asset_class_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::delete_market_series_asset_class_response& v);
    static ores::marketdata::messaging::delete_market_series_asset_class_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::delete_market_series_asset_class_response& v);
    static ores::marketdata::messaging::delete_market_series_asset_class_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::delete_many_market_series_asset_classes_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::marketdata::messaging::delete_many_market_series_asset_classes_request& v);
    static ores::marketdata::messaging::delete_many_market_series_asset_classes_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::marketdata::messaging::delete_many_market_series_asset_classes_request& v);
    static ores::marketdata::messaging::delete_many_market_series_asset_classes_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::delete_many_market_series_asset_classes_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::marketdata::messaging::delete_many_market_series_asset_classes_response& v);
    static ores::marketdata::messaging::delete_many_market_series_asset_classes_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::marketdata::messaging::delete_many_market_series_asset_classes_response& v);
    static ores::marketdata::messaging::delete_many_market_series_asset_classes_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<
    ores::marketdata::messaging::list_by_market_series_id_market_series_asset_classes_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::
                   list_by_market_series_id_market_series_asset_classes_request& v);
    static ores::marketdata::messaging::list_by_market_series_id_market_series_asset_classes_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::
                      list_by_market_series_id_market_series_asset_classes_request& v);
    static ores::marketdata::messaging::list_by_market_series_id_market_series_asset_classes_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<
    ores::marketdata::messaging::list_by_market_series_id_market_series_asset_classes_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::
                   list_by_market_series_id_market_series_asset_classes_response& v);
    static ores::marketdata::messaging::
        list_by_market_series_id_market_series_asset_classes_response
        read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::
                      list_by_market_series_id_market_series_asset_classes_response& v);
    static ores::marketdata::messaging::
        list_by_market_series_id_market_series_asset_classes_response
        read_msgpack(std::span<const std::byte> data);
};
}


#endif
