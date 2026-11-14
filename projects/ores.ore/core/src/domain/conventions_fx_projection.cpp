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
#include "conventions_family_units.hpp"
#include "conventions_fields.hpp"
#include "ores.ore.core/domain/conventions_mapper.hpp"
#include "ores.ore.core/domain/conventions_package.hpp"
#include <cmath>
#include <set>
#include <string>
#include <vector>

namespace ores::ore::domain {

using namespace ores::logging;
using namespace conventions_fields;

namespace conventions_fields {

std::string fx_convention_id(const std::string& base_currency, const std::string& quote_currency) {
    return base_currency + "-" + quote_currency + "-FX-CONVENTIONS";
}

}

ores::refdata::messaging::fx_convention conventions_mapper::map_fx(const record_reader& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping FX convention: " << text_of(v, "Id");

    ores::refdata::messaging::fx_convention r;
    r.id = text_of(v, "Id");
    r.spot_days = integer_of(v, "SpotDays");
    const double points_factor = decimal_of(v, "PointsFactor");

    auto& pair = r.pair;
    pair.base_currency = spelling_of(v, "SourceCurrency", "currency code");
    pair.quote_currency = spelling_of(v, "TargetCurrency", "currency code");
    pair.pair_code = pair.base_currency + "/" + pair.quote_currency;
    set_audit(pair);

    auto& convention = r.convention;
    convention.pair_code = pair.pair_code;
    convention.pip_factor = points_factor != 0.0 ? 1.0 / points_factor : 0.0;
    convention.tick_size = 1.0;
    convention.decimal_places =
        points_factor > 0.0 ? static_cast<int>(std::lround(std::log10(points_factor))) : 0;

    if (const auto joined = opt_text(v, "AdvanceCalendar")) {
        std::size_t start = 0;
        while (start <= joined->size()) {
            const auto comma = joined->find(',', start);
            const auto end = comma == std::string::npos ? joined->size() : comma;
            if (end > start)
                r.advance_calendars.push_back(joined->substr(start, end - start));
            if (comma == std::string::npos)
                break;
            start = comma + 1;
        }
    }

    assign(convention.spot_relative, opt_flag(v, "SpotRelative"));
    assign(convention.end_of_month, opt_flag(v, "EOM"));
    assign(convention.business_day_convention, opt_bdc(v, "Convention"));

    set_audit(convention);
    return r;
}

void conventions_mapper::reverse_fx(const ores::refdata::messaging::fx_convention& v,
                                    record_writer w) {
    w.set_text("Id",
               v.id.empty() ? fx_convention_id(v.pair.base_currency, v.pair.quote_currency) : v.id);
    w.set_integer("SpotDays", v.spot_days);
    put_enum(w, "SourceCurrency", v.pair.base_currency, "currency code");
    put_enum(w, "TargetCurrency", v.pair.quote_currency, "currency code");
    put_decimal(
        w, "PointsFactor", v.convention.pip_factor != 0.0 ? 1.0 / v.convention.pip_factor : 0.0);
    if (!v.advance_calendars.empty())
        w.set_text("AdvanceCalendar", join_with(v.advance_calendars, ','));
    w.set_optional_flag("SpotRelative", v.convention.spot_relative);
    w.set_optional_flag("EOM", v.convention.end_of_month);
    put_bdc(w, "Convention", v.convention.business_day_convention);
}

namespace conventions_family {

std::vector<refdata::messaging::fx_convention> project_fx(const record_reader& root) {
    // An FX convention is a projection of a currency pair, that pair's
    // convention and its advance calendars, so it is kept aside and never
    // packed into a family of its own.
    const auto fx = root.children("FX");
    std::vector<refdata::messaging::fx_convention> out;
    out.reserve(fx.size());
    for (const auto& x : fx)
        out.push_back(conventions_mapper::map_fx(x));
    return out;
}

void join_fx(const std::vector<refdata::messaging::fx_convention>& fx, record_writer& root) {
    for (const auto& v : fx)
        conventions_mapper::reverse_fx(v, root.append("FX"));
}

}

namespace {

/**
 * @brief How plainly @p name spells the pair @p base / @p quote.
 *
 * A name that starts with the pair spells it plainly, and one that states
 * something else first still spells it. A longer pair beats a shorter one, and
 * an earlier spelling beats a later one, so the answer does not depend on the
 * order the store read the names in. Zero when the name does not spell the pair.
 */
std::size_t
plainness_of(const std::string& name, const std::string& base, const std::string& quote) {
    const auto pair = base + "-" + quote;
    if (pair.size() < 3)
        return 0;
    const auto at = name.find(pair);
    if (at == std::string::npos)
        return 0;
    return (at == 0 ? name.size() : name.size() + at) - pair.size() + 1;
}

/**
 * @brief The pair @p name spells, over @p sources, or the end of the list.
 *
 * A name states a source currency and a target currency, and refdata holds one
 * row per pair whichever way round the pair is quoted, so a name either way round
 * names the row.
 */
const fx_pair_source*
pair_named_by(const std::string& name, const std::vector<fx_pair_source>& sources, bool& reversed) {
    const fx_pair_source* best = nullptr;
    std::size_t best_plainness = 0;
    for (const auto& source : sources) {
        for (const bool way : {false, true}) {
            const auto& base = way ? source.quote_currency : source.base_currency;
            const auto& quote = way ? source.base_currency : source.quote_currency;
            const auto plainness = plainness_of(name, base, quote);
            if (plainness > 0 && (best == nullptr || plainness < best_plainness)) {
                best = &source;
                best_plainness = plainness;
                reversed = way;
            }
        }
    }
    return best;
}

}

std::vector<refdata::messaging::fx_convention>
fx_conventions(const std::vector<fx_pair_source>& sources,
               const std::vector<std::string>& references) {
    std::vector<refdata::messaging::fx_convention> out;
    std::set<const fx_pair_source*> named;

    const auto written = [](const fx_pair_source& source, const std::string& id, bool reversed) {
        refdata::messaging::fx_convention c;
        c.id = id;
        c.pair.pair_code = source.pair_code;
        c.pair.base_currency = reversed ? source.quote_currency : source.base_currency;
        c.pair.quote_currency = reversed ? source.base_currency : source.quote_currency;
        c.convention = source.convention;
        c.spot_days = source.spot_days;
        c.advance_calendars = source.advance_calendars;
        conventions_fields::set_audit(c.pair);
        conventions_fields::set_audit(c.convention);
        return c;
    };

    // A name the document already states is a convention of its own, because ORE
    // resolves an FX convention by name and one pair may be named twice.
    std::set<std::string> emitted;
    for (const auto& name : references) {
        if (name.empty() || !emitted.insert(name).second)
            continue;
        bool reversed = false;
        const auto* source = pair_named_by(name, sources, reversed);
        if (source == nullptr)
            continue;
        named.insert(source);
        out.push_back(written(*source, name, reversed));
    }

    // Every other pair the store holds is stated once, under the name refdata's
    // readers expect.
    for (const auto& source : sources) {
        if (named.contains(&source) || source.base_currency.empty() ||
            source.quote_currency.empty())
            continue;
        out.push_back(written(source, {}, false));
    }
    return out;
}

std::vector<std::string> skipped_fx_conventions(const conventions_package& p) {
    std::vector<std::string> names;
    names.reserve(p.fx.size());
    for (const auto& fx : p.fx)
        names.push_back(fx_convention_id(fx.pair.base_currency, fx.pair.quote_currency));
    return names;
}

}
