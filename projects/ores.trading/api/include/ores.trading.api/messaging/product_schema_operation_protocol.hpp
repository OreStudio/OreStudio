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
#ifndef ORES_TRADING_API_MESSAGING_PRODUCT_SCHEMA_OPERATION_PROTOCOL_HPP
#define ORES_TRADING_API_MESSAGING_PRODUCT_SCHEMA_OPERATION_PROTOCOL_HPP

#include "ores.trading.api/domain/script_input.hpp"
#include "ores.trading.api/domain/script_input_code.hpp"
#include "ores.trading.api/export.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include "ores.utility/codec/wire_traits_fwd.hpp"
#include <cstddef>
#include <span>
#include <string>
#include <string_view>
#include <tuple>
#include <vector>

namespace ores::trading::messaging {

/**
 * @brief One field of a schema type a product reaches.
 */
struct product_field {
    /**
     * @brief The schema type the field belongs to, or trade for the data element of a trade type.
     */
    std::string type_name;
    /**
     * @brief The field's element or attribute name, as ORE writes it.
     */
    std::string name;
    /**
     * @brief The field's place among the fields of its type.
     */
    int position = 0;
    /**
     * @brief The field's label.
     */
    std::string label;
    /**
     * @brief One sentence of help.
     */
    std::string help;
    /**
     * @brief What the field means, from the ORE guide where it states it.
     */
    std::string description;
    /**
     * @brief The kind of value: string, boolean, integer, double, date, record:TYPE or list:TYPE.
     */
    std::string value_domain;
    /**
     * @brief Whether a trade must give the field.
     */
    bool is_required = false;
    /**
     * @brief The default, or a sentence saying there is none.
     */
    std::string default_text;
    /**
     * @brief How a value is written, with the values an enumeration allows.
     */
    std::string format;
    /**
     * @brief The field's path relative to its type.
     */
    std::string xpath;

    friend bool operator==(const product_field&, const product_field&) = default;
};

/**
 * @brief One form a product may be booked in.
 */
struct product_form {
    /**
     * @brief native, for the product's own element, or scripted, for a ScriptedTrade over a library
     * script.
     */
    std::string kind;
    /**
     * @brief The library script a scripted form runs; empty for the native form.
     */
    std::string script_name;
    /**
     * @brief The product tag of that script, which a pricing configuration assigns an engine to;
     * empty for the native form.
     */
    std::string product_tag;

    friend bool operator==(const product_form&, const product_form&) = default;
};

/**
 * @brief One product that can be described.
 */
struct product_summary {
    /**
     * @brief The product's code: a product type's code, or a library script's name for a
     * script-only product.
     */
    std::string code;
    /**
     * @brief The product's name.
     */
    std::string name;
    /**
     * @brief How ORE builds the product: native, scripted, script or container; script for a
     * script-only product.
     */
    std::string construction;

    friend bool operator==(const product_summary&, const product_summary&) = default;
};

/**
 * @brief Describes one product: its fields and its forms.
 */
struct describe_product_request {
    using response_type = struct describe_product_response;
    static constexpr std::string_view nats_subject = "trading.v1.ops.describe_product";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    /**
     * @brief The product's code: a product type's code or a library script's name.
     */
    std::string code;

    friend bool operator==(const describe_product_request&,
                           const describe_product_request&) = default;
};

/**
 * @brief The description of a product.
 */
struct describe_product_response {
    /**
     * @brief Whether the product was described.
     */
    bool success = false;
    /**
     * @brief Why it was not, when it was not.
     */
    std::string message;
    /**
     * @brief The product described.
     */
    product_summary product;
    /**
     * @brief The element a trade of the product is read under; empty for a script-only product.
     */
    std::string element;
    /**
     * @brief The fields of the trade's data element and of every type its fields reach, the
     * element's own row first.
     */
    std::vector<product_field> fields;
    /**
     * @brief The inputs of each script a form runs.
     */
    std::vector<ores::trading::domain::script_input> inputs;
    /**
     * @brief The codes of the coded inputs among them.
     */
    std::vector<ores::trading::domain::script_input_code> codes;
    /**
     * @brief The forms the product may be booked in.
     */
    std::vector<product_form> forms;

    friend bool operator==(const describe_product_response&,
                           const describe_product_response&) = default;
};

/**
 * @brief Lists the products that can be described.
 */
struct list_products_request {
    using response_type = struct list_products_response;
    static constexpr std::string_view nats_subject = "trading.v1.ops.list_products";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;

    friend bool operator==(const list_products_request&, const list_products_request&) = default;
};

/**
 * @brief The products that can be described.
 */
struct list_products_response {
    /**
     * @brief Whether the list was read.
     */
    bool success = false;
    /**
     * @brief Why it was not, when it was not.
     */
    std::string message;
    /**
     * @brief Every product type, then every library script no product type twins, each by its code.
     */
    std::vector<product_summary> products;

    friend bool operator==(const list_products_response&, const list_products_response&) = default;
};

/**
 * @brief The members of product_field, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<product_field>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"type_name", &product_field::type_name},
        ::ores::utility::codec::field{"name", &product_field::name},
        ::ores::utility::codec::field{"position", &product_field::position},
        ::ores::utility::codec::field{"label", &product_field::label},
        ::ores::utility::codec::field{"help", &product_field::help},
        ::ores::utility::codec::field{"description", &product_field::description},
        ::ores::utility::codec::field{"value_domain", &product_field::value_domain},
        ::ores::utility::codec::field{"is_required", &product_field::is_required},
        ::ores::utility::codec::field{"default_text", &product_field::default_text},
        ::ores::utility::codec::field{"format", &product_field::format},
        ::ores::utility::codec::field{"xpath", &product_field::xpath},
    };
}

/**
 * @brief The members of product_form, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<product_form>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"kind", &product_form::kind},
        ::ores::utility::codec::field{"script_name", &product_form::script_name},
        ::ores::utility::codec::field{"product_tag", &product_form::product_tag},
    };
}

/**
 * @brief The members of product_summary, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<product_summary>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"code", &product_summary::code},
        ::ores::utility::codec::field{"name", &product_summary::name},
        ::ores::utility::codec::field{"construction", &product_summary::construction},
    };
}

/**
 * @brief The members of describe_product_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<describe_product_request>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"code", &describe_product_request::code},
    };
}

/**
 * @brief The members of describe_product_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<describe_product_response>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"success", &describe_product_response::success},
        ::ores::utility::codec::field{"message", &describe_product_response::message},
        ::ores::utility::codec::field{"product", &describe_product_response::product},
        ::ores::utility::codec::field{"element", &describe_product_response::element},
        ::ores::utility::codec::field{"fields", &describe_product_response::fields},
        ::ores::utility::codec::field{"inputs", &describe_product_response::inputs},
        ::ores::utility::codec::field{"codes", &describe_product_response::codes},
        ::ores::utility::codec::field{"forms", &describe_product_response::forms},
    };
}

/**
 * @brief The members of list_products_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_products_request>) {
    return ::ores::utility::codec::field_list{};
}

/**
 * @brief The members of list_products_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_products_response>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"success", &list_products_response::success},
        ::ores::utility::codec::field{"message", &list_products_response::message},
        ::ores::utility::codec::field{"products", &list_products_response::products},
    };
}

}
namespace ores::utility::codec {

template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::product_field> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::product_field& v);
    static ores::trading::messaging::product_field read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::product_field& v);
    static ores::trading::messaging::product_field read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::product_form> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::product_form& v);
    static ores::trading::messaging::product_form read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::product_form& v);
    static ores::trading::messaging::product_form read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::product_summary> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::product_summary& v);
    static ores::trading::messaging::product_summary read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::product_summary& v);
    static ores::trading::messaging::product_summary read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::describe_product_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::describe_product_request& v);
    static ores::trading::messaging::describe_product_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::describe_product_request& v);
    static ores::trading::messaging::describe_product_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::describe_product_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::describe_product_response& v);
    static ores::trading::messaging::describe_product_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::describe_product_response& v);
    static ores::trading::messaging::describe_product_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::list_products_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::list_products_request& v);
    static ores::trading::messaging::list_products_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::list_products_request& v);
    static ores::trading::messaging::list_products_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::list_products_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::list_products_response& v);
    static ores::trading::messaging::list_products_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::list_products_response& v);
    static ores::trading::messaging::list_products_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
