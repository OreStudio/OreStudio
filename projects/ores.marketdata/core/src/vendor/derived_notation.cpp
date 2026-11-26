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
#include "ores.marketdata.core/vendor/derived_notation.hpp"
#include "notation_support.hpp"
#include "ores.marketdata.api/datum/oresmd_uri_codec.hpp"

namespace ores::marketdata::vendor {

namespace {

constexpr std::string_view derived_fixing_scheme = "oresmd";
constexpr std::string_view derived_fixing_field = "value";
constexpr std::string_view derived_option_scheme = "derived_fx_vol";
constexpr std::string_view derived_option_field = "rate_lnvol";

/**
 * @brief A volatility point of an FX pair, read from the source's own spelling.
 *
 * The pair runs together, then the expiry and the strike label, each after a slash, as in
 * USDCZK/2M/10BF. The strike labels are the five a broker shows, and a pair is six letters, so a
 * point and a fixing can never be read from one another.
 */
bool parse_derived_option(std::string_view text, fx_option_quote& option) {
    const auto grammar =
        detail::currency_code[detail::assign_to(option.base)] >>
        detail::currency_code[detail::assign_to(option.quote)] >> detail::x3::lit('/') >>
        detail::period_tenor[detail::assign_to(option.expiry)] >> detail::x3::lit('/') >>
        detail::strike_label[detail::assign_to(option.strike_label)];
    return detail::parse_all(text, grammar) && option.base != option.quote;
}

/// The point @p option names, in the source's own spelling.
std::string write_derived_option(const fx_option_quote& option) {
    return option.base + option.quote + "/" + option.expiry + "/" + option.strike_label;
}

/**
 * @brief The fixing @p item names, in the codec's own spelling, or why it names none.
 *
 * The codec writes an address in one spelling, so an item that reads back in another spelling
 * is refused: two ids would otherwise name one fixing.
 */
std::expected<std::string, notation_error> canonical_derived_fixing(const std::string& item) {
    using detail::fail;
    const auto index = datum::oresmd_uri_codec::read_index(item);
    if (!index)
        return fail(notation_error::kind::unsupported,
                    "'" + item + "' names no fixing: " + index.error());
    const auto written = datum::oresmd_uri_codec::write_index(*index);
    if (!written)
        return fail(notation_error::kind::not_representable,
                    "'" + item + "' cannot be written back: " + written.error());
    if (*written != item)
        return fail(notation_error::kind::malformed,
                    "'" + item + "' is not written as the codec writes it, '" + *written + "'.");
    return *written;
}

}

std::string_view derived_notation::provider() const {
    return derived_provider;
}

std::string_view derived_notation::id_scheme() const {
    return derived_fixing_scheme;
}

price_side derived_notation::primary_side() const {
    return price_side::last;
}

std::expected<quote_description, notation_error>
derived_notation::read(const messaging::vendor_id& id) const {
    using detail::fail;
    if (id.id_scheme == derived_option_scheme) {
        if (id.field != derived_option_field)
            return fail(notation_error::kind::wrong_field,
                        "The field '" + id.field + "' is not '" +
                            std::string(derived_option_field) + "'.");
        fx_option_quote option;
        if (!parse_derived_option(id.external_id, option))
            return fail(notation_error::kind::malformed,
                        "'" + id.external_id +
                            "' names no volatility point: a six letter pair, an expiry and one "
                            "of ATM, 25RR, 10RR, 25BF or 10BF, each after a slash.");
        return quote_description{option};
    }
    if (id.id_scheme != derived_fixing_scheme)
        return fail(notation_error::kind::wrong_scheme,
                    "The scheme '" + id.id_scheme + "' is neither '" +
                        std::string(derived_fixing_scheme) + "' nor '" +
                        std::string(derived_option_scheme) + "'.");
    if (id.field != derived_fixing_field)
        return fail(notation_error::kind::wrong_field,
                    "The field '" + id.field + "' is not '" + std::string(derived_fixing_field) +
                        "'.");
    const auto item = canonical_derived_fixing(id.external_id);
    if (!item)
        return std::unexpected(item.error());
    return quote_description{index_fixing{.item = *item}};
}

std::expected<messaging::vendor_id, notation_error>
derived_notation::write(const quote_description& description) const {
    using detail::fail;
    if (const auto* option = std::get_if<fx_option_quote>(&description)) {
        fx_option_quote read_back;
        const auto written = write_derived_option(*option);
        if (!parse_derived_option(written, read_back) || read_back != *option)
            return fail(notation_error::kind::not_representable,
                        "The derived source writes a point as a six letter pair, an expiry and "
                        "one of ATM, 25RR, 10RR, 25BF or 10BF: " +
                            written + ".");
        return messaging::vendor_id{.id_scheme = std::string(derived_option_scheme),
                                    .external_id = written,
                                    .field = std::string(derived_option_field)};
    }
    const auto* fixing = std::get_if<index_fixing>(&description);
    if (fixing == nullptr)
        return fail(notation_error::kind::not_representable,
                    "The derived provider writes fixings and volatility points, not this quote.");
    const auto item = canonical_derived_fixing(fixing->item);
    if (!item)
        return std::unexpected(item.error());
    return messaging::vendor_id{.id_scheme = std::string(derived_fixing_scheme),
                                .external_id = *item,
                                .field = std::string(derived_fixing_field)};
}

}
