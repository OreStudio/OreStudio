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
#include "conventions_family_pack.hpp"
#include "conventions_family_units.hpp"
#include "conventions_fields.hpp"
#include "ores.ore.core/domain/conventions_mapper.hpp"

namespace ores::ore::domain {

using namespace ores::logging;
using namespace conventions_fields;

refdata::domain::fx_option_convention conventions_mapper::map_fx_option(const record_reader& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping FX option convention: " << text_of(v, "Id");

    refdata::domain::fx_option_convention r;
    r.id = text_of(v, "Id");
    assign(r.fx_convention_id, opt_text(v, "FXConventionID"));
    r.atm_type = text_of(v, "AtmType");
    r.delta_type = text_of(v, "DeltaType");
    assign(r.switch_tenor, opt_text(v, "SwitchTenor"));
    assign(r.long_term_atm_type, opt_text(v, "LongTermAtmType"));
    assign(r.long_term_delta_type, opt_text(v, "LongTermDeltaType"));
    assign(r.risk_reversal_in_favor_of, opt_text(v, "RiskReversalInFavorOf"));
    assign(r.butterfly_style, opt_text(v, "ButterflyStyle"));
    set_audit(r);
    return r;
}

void conventions_mapper::reverse_fx_option(const refdata::domain::fx_option_convention& v,
                                           record_writer w) {
    w.set_text("Id", v.id);
    w.set_optional_text("FXConventionID", v.fx_convention_id);
    w.set_text("AtmType", v.atm_type);
    w.set_text("DeltaType", v.delta_type);
    w.set_optional_text("SwitchTenor", v.switch_tenor);
    w.set_optional_text("LongTermAtmType", v.long_term_atm_type);
    w.set_optional_text("LongTermDeltaType", v.long_term_delta_type);
    w.set_optional_text("RiskReversalInFavorOf", v.risk_reversal_in_favor_of);
    w.set_optional_text("ButterflyStyle", v.butterfly_style);
}

namespace conventions_family {

void pack_fx_option(const record_reader& root, refdata::service::conventions_bundle& bundle) {
    pack_family(bundle, "fx_option", root.children("FxOption"), &conventions_mapper::map_fx_option);
}

void join_fx_option(const package_row& row, record_writer& root) {
    conventions_mapper::reverse_fx_option(unpack_row<refdata::domain::fx_option_convention>(row),
                                          root.append("FxOption"));
}

}

}
