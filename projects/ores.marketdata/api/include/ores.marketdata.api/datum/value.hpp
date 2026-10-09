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
#ifndef ORES_MARKETDATA_API_DATUM_VALUE_HPP
#define ORES_MARKETDATA_API_DATUM_VALUE_HPP

#include "ores.marketdata.api/export.hpp"
#include <cstdint>
#include <expected>
#include <optional>
#include <string>
#include <string_view>
#include <variant>

/**
 * @file value.hpp
 * @brief The values a market datum's fields hold.
 *
 * A value keeps the text the key carried, so a datum writes back exactly the
 * key it was read from. Parsing checks the text's form; it never rewrites it,
 * and in particular it never changes its case.
 *
 * A strike label is the one exception. It is a component of a composite
 * object, and two spellings of one component must be one value, so the label is
 * stored as a canonical code. See strike_label.
 */

namespace ores::marketdata::datum {

/**
 * @brief The value of a field the key does not carry.
 *
 * An explicit value rather than a missing field, so that the absence is stated
 * and never stands for a default.
 */
struct none_t {
    friend bool operator==(none_t, none_t) = default;
};

inline constexpr none_t none{};

/**
 * @brief A point in time as ORE's keys write one: a period, a date, an FX
 * overnight tenor, or a futures continuation.
 *
 * One type because ORE lets one key slot hold any of them: a discount factor's
 * pillar is a period or a date, and a commodity forward's is a period, a date or
 * ON, TN or SN.
 */
class ORES_MARKETDATA_API_EXPORT term final {
public:
    enum class kind : std::uint8_t {
        /// One or more length-unit pairs: 1Y, 6M, 0D, 1Y6M.
        period,
        /// YYYY-MM-DD or YYYYMMDD.
        date,
        /// ON, TN or SN.
        fx_tenor,
        /// c followed by a count: c1 is the nearest futures contract.
        continuation
    };

    [[nodiscard]] static std::expected<term, std::string> parse(std::string_view text);

    [[nodiscard]] kind which() const noexcept {
        return kind_;
    }
    [[nodiscard]] const std::string& text() const noexcept {
        return text_;
    }

    friend bool operator==(const term&, const term&) = default;

private:
    term(kind k, std::string text);

    kind kind_;
    std::string text_;
};

/**
 * @brief A number as the key wrote it, in the grammar
 * ores::platform::numeric::parse_double accepts.
 *
 * Kept as text, because converting to a double and back would not give the
 * key's own spelling: 0.030 would come back as 0.03.
 */
class ORES_MARKETDATA_API_EXPORT decimal final {
public:
    [[nodiscard]] static std::expected<decimal, std::string> parse(std::string_view text);

    [[nodiscard]] const std::string& text() const noexcept {
        return text_;
    }

    friend bool operator==(const decimal&, const decimal&) = default;

private:
    explicit decimal(std::string text);

    std::string text_;
};

/**
 * @brief A token from a closed vocabulary ORE checks, such as C or F for a cap
 * or a floor.
 *
 * The vocabulary belongs to the field, so the schema validates it; the code only
 * requires a non-empty token.
 */
class ORES_MARKETDATA_API_EXPORT code final {
public:
    [[nodiscard]] static std::expected<code, std::string> parse(std::string_view text);

    [[nodiscard]] const std::string& text() const noexcept {
        return text_;
    }

    friend bool operator==(const code&, const code&) = default;

private:
    explicit code(std::string text);

    std::string text_;
};

/// A strike at a level: 3000, 0.03.
struct absolute_strike {
    decimal level;
    friend bool operator==(const absolute_strike&, const absolute_strike&) = default;
};

/**
 * @brief An at-the-money strike: ATM/<atm type>, optionally with /DEL/<delta
 * type>.
 *
 * An equity option may also write ATM for ATM/AtmSpot and ATMF for ATM/AtmFwd.
 * The shorthand is kept, so the key reads back as it was written.
 */
struct atm_strike {
    std::string atm_type;
    std::optional<std::string> delta_type;
    bool shorthand = false;
    friend bool operator==(const atm_strike&, const atm_strike&) = default;
};

/// A delta strike: DEL/<delta type>/<Call|Put>/<delta>.
struct delta_strike {
    std::string delta_type;
    std::string option_type;
    decimal delta;
    friend bool operator==(const delta_strike&, const delta_strike&) = default;
};

/// A moneyness strike: MNY/<Spot|Fwd>/<moneyness>.
struct moneyness_strike {
    std::string moneyness_type;
    decimal moneyness;
    friend bool operator==(const moneyness_strike&, const moneyness_strike&) = default;
};

/**
 * @brief A strike in ORE's strike grammar: the form ORE's parseBaseStrike reads,
 * and the ATM and ATMF shorthand its equity option parser adds.
 */
class ORES_MARKETDATA_API_EXPORT strike final {
public:
    using form = std::variant<absolute_strike, atm_strike, delta_strike, moneyness_strike>;

    [[nodiscard]] static std::expected<strike, std::string> parse(std::string_view text);

    [[nodiscard]] const form& which() const noexcept {
        return form_;
    }

    /// The strike as ORE writes it, which is the text it was parsed from.
    [[nodiscard]] std::string text() const;

    friend bool operator==(const strike&, const strike&) = default;

private:
    explicit strike(form f);

    form form_;
};

/**
 * @brief An FX option strike quoted as a label: ATM, a delta wing, a delta call
 * or a delta put, or a level.
 *
 * ORE's FX option quote keeps the strike as a label, and the label is the
 * second axis of a volatility surface: ATM, 25RR and 25BF name one component
 * each. The label is stored in its canonical spelling, so the case of its
 * suffix is fixed, a leading + is dropped and a fraction of zeros is dropped:
 * 25rr, 25RR, +25RR and 25.0RR are one value. The digits are otherwise kept as
 * the key wrote them, because rewriting a number changes the value and not only
 * its spelling.
 *
 * The forms are the ones ORE's FXOptionQuote admits. A form its strike grammar
 * reads and its FX option quote refuses, such as ATMF or 25D, is refused here
 * too.
 */
class ORES_MARKETDATA_API_EXPORT strike_label final {
public:
    enum class form : std::uint8_t {
        /// ATM: at the money.
        at_the_money,
        /// 25C: a call at a delta.
        delta_call,
        /// 25P: a put at a delta.
        delta_put,
        /// 25RR: a risk reversal at a delta.
        risk_reversal,
        /// 25BF: a butterfly at a delta.
        butterfly,
        /// 0.05: a level the quote states outright.
        level
    };

    [[nodiscard]] static std::expected<strike_label, std::string> parse(std::string_view text);

    [[nodiscard]] form which() const noexcept {
        return form_;
    }

    /// The delta or the level; empty for ATM.
    [[nodiscard]] const std::string& number() const noexcept {
        return number_;
    }

    /// The label in its canonical spelling.
    [[nodiscard]] std::string text() const;

    friend bool operator==(const strike_label&, const strike_label&) = default;

private:
    strike_label(form f, std::string number);

    form form_;
    std::string number_;
};

/**
 * @brief Whatever a market datum field holds.
 *
 * Free text, such as a name or a currency, is a string kept in the case it
 * arrived in. A field states which of these it holds; see schema.hpp.
 */
using value = std::variant<none_t, std::string, term, decimal, strike, code, strike_label>;

/// The value as the key writes it, or the empty string for none.
ORES_MARKETDATA_API_EXPORT std::string text_of(const value& v);

}

#endif
