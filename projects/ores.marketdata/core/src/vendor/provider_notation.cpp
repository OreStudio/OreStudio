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
#include "ores.marketdata.core/vendor/provider_notation.hpp"
#include "ores.marketdata.core/vendor/bloomberg_notation.hpp"
#include "ores.marketdata.core/vendor/demo_notation.hpp"
#include "ores.marketdata.core/vendor/derived_notation.hpp"
#include "ores.marketdata.core/vendor/fixing_setting_notation.hpp"
#include "ores.marketdata.core/vendor/ice_notation.hpp"
#include "ores.marketdata.core/vendor/lseg_notation.hpp"
#include "ores.marketdata.core/vendor/simulated_fixing_notation.hpp"
#include "ores.marketdata.core/vendor/tradeweb_notation.hpp"
#include "ores.marketdata.core/vendor/wmr_cut_notation.hpp"

namespace ores::marketdata::vendor {

const provider_notation* notation_of(std::string_view provider) {
    static const bloomberg_notation bloomberg;
    static const lseg_notation lseg;
    static const ice_notation ice;
    static const tradeweb_notation tradeweb;
    static const demo_notation demo;
    static const simulated_fixing_notation simulated_fixings;
    static const derived_notation derived;
    static const wmr_cut_notation wmr;
    static const fixing_setting_notation iba{"iba"};
    static const fixing_setting_notation emmi{"emmi"};
    static const fixing_setting_notation hkab{"hkab"};
    static const fixing_setting_notation gpw{"gpw"};
    static const fixing_setting_notation banrep{"banrep"};
    static const fixing_setting_notation snb{"snb"};
    static const fixing_setting_notation danmarks_nationalbank{"danmarks-nationalbank"};
    static const fixing_setting_notation mnb{"mnb"};
    static const fixing_setting_notation bok{"bok"};
    static const fixing_setting_notation nbp{"nbp"};
    static const fixing_setting_notation rbnz{"rbnz"};
    static const fixing_setting_notation sarb{"sarb"};
    static const fixing_setting_notation boi{"boi"};
    static const fixing_setting_notation nifc{"nifc"};
    static const fixing_setting_notation fbil{"fbil"};
    static const fixing_setting_notation tba{"tba"};
    static const fixing_setting_notation bist{"bist"};
    for (const provider_notation* candidate :
         {static_cast<const provider_notation*>(&bloomberg),
          static_cast<const provider_notation*>(&lseg),
          static_cast<const provider_notation*>(&ice),
          static_cast<const provider_notation*>(&tradeweb),
          static_cast<const provider_notation*>(&demo),
          static_cast<const provider_notation*>(&simulated_fixings),
          static_cast<const provider_notation*>(&derived),
          static_cast<const provider_notation*>(&wmr),
          static_cast<const provider_notation*>(&iba),
          static_cast<const provider_notation*>(&emmi),
          static_cast<const provider_notation*>(&hkab),
          static_cast<const provider_notation*>(&gpw),
          static_cast<const provider_notation*>(&banrep),
          static_cast<const provider_notation*>(&snb),
          static_cast<const provider_notation*>(&danmarks_nationalbank),
          static_cast<const provider_notation*>(&mnb),
          static_cast<const provider_notation*>(&bok),
          static_cast<const provider_notation*>(&nbp),
          static_cast<const provider_notation*>(&rbnz),
          static_cast<const provider_notation*>(&sarb),
          static_cast<const provider_notation*>(&boi),
          static_cast<const provider_notation*>(&nifc),
          static_cast<const provider_notation*>(&fbil),
          static_cast<const provider_notation*>(&tba),
          static_cast<const provider_notation*>(&bist)})
        if (candidate->provider() == provider)
            return candidate;
    return nullptr;
}

}
