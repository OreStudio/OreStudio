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
#ifndef ORES_ORE_CORE_DOMAIN_PAYMENT_FREQUENCY_CONVERSION_HPP
#define ORES_ORE_CORE_DOMAIN_PAYMENT_FREQUENCY_CONVERSION_HPP

#include <string>

namespace ores::ore::domain {

/**
 * @brief Converts an ORE schedule rule's Tenor period string (e.g. "3M",
 * "1Y", "1D") into the canonical ores.refdata.payment_frequency code name
 * (e.g. "Quarterly", "Annual", "Daily").
 *
 * ORE schedule XML always expresses frequency as a period string, never as
 * the named enum ores.refdata.payment_frequency validates against, so every
 * importer that stamps a payment_frequency FK column from schedule data must
 * convert through this function rather than storing the raw tenor.
 *
 * Unrecognised inputs are passed through unchanged so the caller's own FK
 * validation surfaces the problem rather than this function guessing.
 */
inline std::string tenor_to_payment_frequency(const std::string& tenor) {
    if (tenor == "1D" || tenor == "0D")
        return "Daily";
    if (tenor == "1W")
        return "Weekly";
    if (tenor == "1M")
        return "Monthly";
    if (tenor == "2M")
        return "Bimonthly";
    if (tenor == "3M")
        return "Quarterly";
    if (tenor == "6M")
        return "Semiannual";
    if (tenor == "1Y" || tenor == "12M")
        return "Annual";
    return tenor;
}

/**
 * @brief Converts a canonical ores.refdata.payment_frequency code name back
 * into the ORE schedule rule Tenor period string it was derived from.
 *
 * Inverse of tenor_to_payment_frequency(); used by reverse-mappers producing
 * ORE XML from persisted instrument state.
 */
inline std::string payment_frequency_to_tenor(const std::string& frequency) {
    if (frequency == "Once")
        return "0D";
    if (frequency == "Daily")
        return "1D";
    if (frequency == "Weekly")
        return "1W";
    if (frequency == "Monthly")
        return "1M";
    if (frequency == "Bimonthly")
        return "2M";
    if (frequency == "Quarterly")
        return "3M";
    if (frequency == "Semiannual")
        return "6M";
    if (frequency == "Annual")
        return "1Y";
    if (frequency == "Lunarmonth")
        return "28D";
    return frequency;
}

}

#endif
