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
#ifndef ORES_TRADING_API_BUILDERS_REQUIRED_HPP
#define ORES_TRADING_API_BUILDERS_REQUIRED_HPP

#include <utility>

namespace ores::trading::builders {

/**
 * @brief A term a trade cannot be built without.
 *
 * It has no default constructor, so a terms aggregate that leaves one out does
 * not compile.
 */
template <typename T>
class required final {
public:
    required() = delete;

    template <typename U = T>
    required(U&& value) : value_(std::forward<U>(value)) {}

    [[nodiscard]] const T& get() const { return value_; }

private:
    T value_;
};

}

#endif
