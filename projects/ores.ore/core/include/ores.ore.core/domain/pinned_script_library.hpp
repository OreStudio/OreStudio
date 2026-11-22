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
#ifndef ORES_ORE_CORE_DOMAIN_PINNED_SCRIPT_LIBRARY_HPP
#define ORES_ORE_CORE_DOMAIN_PINNED_SCRIPT_LIBRARY_HPP

#include "ores.ore.core/export.hpp"
#include "ores.trading.api/domain/instrument_batch.hpp"
#include "ores.trading.api/messaging/script_library_operation_protocol.hpp"
#include <string>
#include <vector>

namespace ores::ore::domain {

/**
 * @brief The script library a run's scripted trades need, by the versions they
 * were booked on.
 *
 * ORE finds a script by name alone, so a run holds one script per name. When
 * the trades of a run name one script at two versions, each version is written
 * under a name of its own, and the trades that state their variables in the
 * generic form are given that name. A trade that states them on the product's
 * own element cannot be renamed, as ORE takes the script's name from the
 * element, so those trades must agree on one version.
 */
struct pinned_script_library final {
    /**
     * @brief One version the run needs, and the name the run's library gives it.
     */
    struct entry final {
        trading::messaging::script_pin pin;
        std::string library_name;
    };

    /// The versions the run needs, each once, by name, tenant and version.
    std::vector<entry> entries;

    /// Why the run cannot be given its scripts, one sentence for each reason.
    std::vector<std::string> refusals;
};

/**
 * @brief Plans the library of a batch's scripted trades, and renames the
 * generic trades whose script the run holds at more than one version.
 *
 * A trade that states its script inline needs no library. A trade that names a
 * script no library held when it was booked has no version, and is a refusal.
 */
ORES_ORE_CORE_EXPORT pinned_script_library
pin_script_library(trading::domain::instrument_batch& batch);

/**
 * @brief The versions to read for a plan.
 */
ORES_ORE_CORE_EXPORT std::vector<trading::messaging::script_pin>
pins_of(const pinned_script_library& plan);

/**
 * @brief Writes the scriptlibrary.xml document of a plan from the rows read for it.
 *
 * Each script is written under the name the plan gives its version. A row of a
 * version the plan does not hold is not written.
 */
ORES_ORE_CORE_EXPORT std::string
write_pinned_script_library(const pinned_script_library& plan,
                            const trading::messaging::export_script_library_response& rows);

}

#endif
