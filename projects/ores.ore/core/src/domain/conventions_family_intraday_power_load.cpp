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
#include <optional>
#include <string>

namespace ores::ore::domain {

using namespace ores::logging;
using namespace conventions_fields;

namespace conventions_fields {

std::string serialize_factors(const std::optional<record_reader>& factors) {
    std::string out;
    if (!factors)
        return out;
    for (const auto& f : factors->children("LoadFactor")) {
        if (!out.empty())
            out += ",";
        out += text_of(f, "from") + ":" + text_of(f, "to") + ":" + text_of(f, "unit") + ":" +
               text_flag_field(f, "dst") + ":" + f.own_text().value_or(std::string());
    }
    return out;
}

void write_factors(const std::string& s, record_writer factors) {
    for (const auto& record : split_fields(s, ',')) {
        if (record.empty())
            continue;
        const auto parts = split_fields(record, ':');
        if (parts.size() < 5)
            continue;
        auto f = factors.append("LoadFactor");
        f.set_text("from", parts[0]);
        f.set_text("to", parts[1]);
        if (!parts[2].empty())
            f.set_text("unit", parts[2]);
        if (!parts[3].empty())
            f.set_text("dst", parts[3]);
        f.set_own_text(parts[4]);
    }
}

}

refdata::domain::intraday_power_load_convention
conventions_mapper::map_intraday_power_load(const record_reader& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping intraday power load convention: " << text_of(v, "Id");

    refdata::domain::intraday_power_load_convention r;
    r.id = text_of(v, "Id");

    if (const auto profile = v.child("PowerLoadProfileData")) {
        if (const auto explicit_dates = profile->child("ExplicitDates")) {
            std::string out;
            for (const auto& datum : explicit_dates->children("LoadProfileDatum")) {
                if (!out.empty())
                    out += ";";
                out += text_of(datum, "Date") + "|" + serialize_factors(datum.child("LoadFactors"));
            }
            r.explicit_load_profile = out;
        }
        if (const auto rules = profile->child("BusinessDayRules")) {
            std::string out;
            for (const auto& rule : rules->children("LoadProfileBusinessDayRule")) {
                if (!out.empty())
                    out += ";";
                out += text_of(rule, "Date") + "|" + text_of(rule, "Calendar") + "|" +
                       serialize_factors(rule.child("BusinessDayLoadFactors")) + "|" +
                       serialize_factors(rule.child("NonBusinessDayLoadFactors"));
            }
            r.business_day_load_rules = out;
        }
    }

    set_audit(r);
    return r;
}

void conventions_mapper::reverse_intraday_power_load(
    const refdata::domain::intraday_power_load_convention& v, record_writer w) {
    w.set_text("Id", v.id);
    if (!v.explicit_load_profile && !v.business_day_load_rules)
        return;

    auto profile = w.child("PowerLoadProfileData");
    if (v.explicit_load_profile) {
        auto explicit_dates = profile.child("ExplicitDates");
        for (const auto& record : split_fields(*v.explicit_load_profile, ';')) {
            if (record.empty())
                continue;
            const auto parts = split_fields(record, '|');
            auto datum = explicit_dates.append("LoadProfileDatum");
            datum.set_text("Date", parts.empty() ? std::string() : parts[0]);
            auto factors = datum.child("LoadFactors");
            if (parts.size() > 1)
                write_factors(parts[1], factors);
        }
    }
    if (v.business_day_load_rules) {
        auto rules = profile.child("BusinessDayRules");
        for (const auto& record : split_fields(*v.business_day_load_rules, ';')) {
            const auto parts = split_fields(record, '|');
            auto rule = rules.append("LoadProfileBusinessDayRule");
            rule.set_text("Date", parts.empty() ? std::string() : parts[0]);
            rule.set_text("Calendar", parts.size() > 1 ? parts[1] : std::string());
            if (parts.size() > 2 && !parts[2].empty())
                write_factors(parts[2], rule.child("BusinessDayLoadFactors"));
            if (parts.size() > 3 && !parts[3].empty())
                write_factors(parts[3], rule.child("NonBusinessDayLoadFactors"));
        }
    }
}

namespace conventions_family {

void pack_intraday_power_load(const record_reader& root,
                              refdata::service::conventions_bundle& bundle) {
    pack_family(bundle,
                "intraday_power_load",
                root.children("IntradayPowerLoad"),
                &conventions_mapper::map_intraday_power_load);
}

void join_intraday_power_load(const package_row& row, record_writer& root) {
    conventions_mapper::reverse_intraday_power_load(
        unpack_row<refdata::domain::intraday_power_load_convention>(row),
        root.append("IntradayPowerLoad"));
}

}

}
