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
#ifndef ORES_ORE_MODEL_CURRENCY_CONFIG_HPP
#define ORES_ORE_MODEL_CURRENCY_CONFIG_HPP

#if defined(_MSC_VER) && (_MSC_VER >= 1200)
#pragma once
#endif

#include <iosfwd>
#include <vector>
#include "ores.ore/model/currency.hpp"

namespace ores::ore::model {

struct currency_config {
public:
    std::vector<currency> currencies() const { return currencies_; }
    void currencies(const std::vector<currency>& currencies) {
        currencies_ = currencies;
    }

private:
    std::vector<currency> currencies_;
};

std::ostream& operator<<(std::ostream& s, const currency_config& v);

}

#endif
