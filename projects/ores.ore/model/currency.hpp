/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2024 Marco Craveiro <marco.craveiro@gmail.com>
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
#ifndef ORES_ORE_MODEL_CURRENCY_HPP
#define ORES_ORE_MODEL_CURRENCY_HPP

#if defined(_MSC_VER) && (_MSC_VER >= 1200)
#pragma once
#endif

#include <string>

namespace ores::ore::model {

class currency {
public:
    std::string name() const { return name_; }
    void name(const std::string& name) { name_ = name; }

    std::string iso_code() const { return iso_code_; }
    void iso_code(const std::string& iso_code) { iso_code_ = iso_code; }

    int numeric_code() const { return numeric_code_; }
    void numeric_code(int numeric_code) { numeric_code_ = numeric_code; }

    std::string symbol() const { return symbol_; }
    void symbol(const std::string& symbol) { symbol_ = symbol; }

    std::string fraction_symbol() const { return fraction_symbol_; }
    void fraction_symbol(const std::string& fraction_symbol) {
        fraction_symbol_ = fraction_symbol;
    }

    int fractions_per_unit() const { return fractions_per_unit_; }
    void fractions_per_unit(int fractions_per_unit) {
        fractions_per_unit_ = fractions_per_unit;
    }

    std::string rounding_type() const { return rounding_type_; }
    void rounding_type(const std::string& rounding_type) {
        rounding_type_ = rounding_type;
    }

    int rounding_precision() const { return rounding_precision_; }
    void rounding_precision(int rounding_precision) {
        rounding_precision_ = rounding_precision;
    }

    std::string format() const { return format_; }
    void format(const std::string& format) { format_ = format; }

private:
    std::string name_;
    std::string iso_code_;
    int numeric_code_;
    std::string symbol_;
    std::string fraction_symbol_;
    int fractions_per_unit_;
    std::string rounding_type_;
    int rounding_precision_;
    std::string format_;
};

std::ostream& operator<<(std::ostream& s, const currency& v);

}

#endif
