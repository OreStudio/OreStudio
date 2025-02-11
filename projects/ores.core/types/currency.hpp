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
#ifndef ORES_CORE_TYPES_CURRENCY_HPP
#define ORES_CORE_TYPES_CURRENCY_HPP

#if defined(_MSC_VER) && (_MSC_VER >= 1200)
#pragma once
#endif

#include <string>

namespace ores::core::types {

class currency {
public:
    std::string name() const { return name_; }
    void name(const std::string& v) { name_ = v; }

    std::string iso_code() const { return iso_code_; }
    void iso_code(const std::string& v) { iso_code_ = v; }

    int numeric_code() const { return numeric_code_; }
    void numeric_code(int v) { numeric_code_ = v; }

    std::string symbol() const { return symbol_; }
    void symbol(const std::string& v) { symbol_ = v; }

    std::string fraction_symbol() const { return fraction_symbol_; }
    void fraction_symbol(const std::string& v) {
        fraction_symbol_ = v;
    }

    int fractions_per_unit() const { return fractions_per_unit_; }
    void fractions_per_unit(int v) { fractions_per_unit_ = v; }

    std::string rounding_type() const { return rounding_type_; }
    void rounding_type(const std::string& v) { rounding_type_ = v; }

    int rounding_precision() const { return rounding_precision_; }
    void rounding_precision(int v) { rounding_precision_ = v; }

    std::string format() const { return format_; }
    void format(const std::string& v) { format_ = v; }

    std::string currency_type() const { return currency_type_; }
    void currency_type(const std::string& v) { currency_type_ = v; }

    std::string modified_by() const { return modified_by_; }
    void modified_by(const std::string& v) { modified_by_ = v; }

    std::string valid_from() const { return valid_from_; }
    void valid_from(std::string v) { valid_from_ = v; }

    std::string valid_to() const { return valid_to_; }
    void valid_to(std::string v) { valid_to_ = v; }

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
    std::string currency_type_;
    std::string modified_by_;
    std::string valid_from_;
    std::string valid_to_;
};

std::ostream& operator<<(std::ostream& s, const currency& v);

}

#endif
