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
#include "ores.utility/uuid/uuid_v7_generator.hpp"
#include <format>
#include <stdexcept>
#include <string>
#include <vector>

namespace ores::ore::domain {

using namespace ores::logging;
using namespace conventions_fields;

namespace conventions_fields {

// A convention is identified by its party, its kind and the name it carries in
// the source document. The mapper knows the kind and the name but not the
// party, so it cannot state that identity and does not try: the store resolves
// it once the session's party is known. What the mapper emits instead is a
// provisional key, minted fresh for each convention it maps, so the part rows
// can reach their central row inside one mapped document. The store discards
// the provisional key and writes the row under the key the identity resolves
// to, so the provisional key is never an identity.
boost::uuids::uuid provisional_uuid() {
    static thread_local ores::utility::uuid::uuid_v7_generator generator;
    return generator();
}

}

ores::refdata::messaging::tenor_basis_swap_convention_item
conventions_mapper::map_tenor_basis_swap(const record_reader& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping tenor basis swap convention: " << text_of(v, "Id");

    ores::refdata::messaging::tenor_basis_swap_convention_item r;
    refdata::domain::convention central;
    central.identity.kind_code = std::string(conventions_family::tenor_basis_swap_kind);
    central.payload.name = text_of(v, "Id");
    central.identity.id = provisional_uuid();
    set_audit(central);
    r.key.id = central.identity.id;
    r.convention = central;

    // The form comes from the leg fields alone. IncludeSpread and
    // SubPeriodsCouponType are written alongside both forms, so they must never
    // decide it.
    const bool pay_receive_form = v.has("PayIndex") || v.has("ReceiveIndex");
    if (pay_receive_form) {
        refdata::domain::tenor_basis_swap_pay_receive p;
        p.identity.id = provisional_uuid();
        p.identity.convention_id = central.identity.id;
        assign(p.pay_leg.pay_index, opt_text(v, "PayIndex"));
        assign(p.pay_leg.pay_frequency, opt_text(v, "PayFrequency"));
        assign(p.rec_leg.receive_index, opt_text(v, "ReceiveIndex"));
        assign(p.rec_leg.receive_frequency, opt_text(v, "ReceiveFrequency"));
        assign(p.coupon.spread_on_rec, opt_flag(v, "SpreadOnRec"));
        assign(p.coupon.include_spread, opt_flag(v, "IncludeSpread"));
        assign(p.coupon.sub_periods_coupon_type, opt_spelling(v, "SubPeriodsCouponType"));
        assign(p.pay_leg.pay_is_averaged, opt_flag(v, "PayIsAveraged"));
        assign(p.rec_leg.rec_is_averaged, opt_flag(v, "RecIsAveraged"));
        set_audit(p);
        r.pay_receive.push_back(std::move(p));
        return r;
    }

    refdata::domain::tenor_basis_swap_long_short l;
    l.identity.id = provisional_uuid();
    l.identity.convention_id = central.identity.id;
    assign(l.long_leg.long_index, opt_text(v, "LongIndex"));
    assign(l.long_leg.long_pay_tenor, opt_text(v, "LongPayTenor"));
    assign(l.short_leg.short_index, opt_text(v, "ShortIndex"));
    assign(l.short_leg.short_pay_tenor, opt_text(v, "ShortPayTenor"));
    assign(l.short_leg.spread_on_short, opt_flag(v, "SpreadOnShort"));
    assign(l.coupon.include_spread, opt_flag(v, "IncludeSpread"));
    assign(l.coupon.sub_periods_coupon_type, opt_spelling(v, "SubPeriodsCouponType"));
    set_audit(l);
    r.long_short.push_back(std::move(l));
    return r;
}

void conventions_mapper::reverse_tenor_basis_swap(
    const ores::refdata::messaging::tenor_basis_swap_convention_item& v, record_writer w) {
    if (v.convention)
        w.set_text("Id", v.convention->payload.name);
    if (!v.pay_receive.empty()) {
        const auto& p = v.pay_receive.front();
        w.set_optional_text("PayIndex", p.pay_leg.pay_index);
        w.set_optional_text("PayFrequency", p.pay_leg.pay_frequency);
        w.set_optional_text("ReceiveIndex", p.rec_leg.receive_index);
        w.set_optional_text("ReceiveFrequency", p.rec_leg.receive_frequency);
        w.set_optional_flag("SpreadOnRec", p.coupon.spread_on_rec);
        w.set_optional_flag("IncludeSpread", p.coupon.include_spread);
        put_enum(
            w, "SubPeriodsCouponType", p.coupon.sub_periods_coupon_type, "sub-periods coupon type");
        w.set_optional_flag("PayIsAveraged", p.pay_leg.pay_is_averaged);
        w.set_optional_flag("RecIsAveraged", p.rec_leg.rec_is_averaged);
        return;
    }
    if (!v.long_short.empty()) {
        const auto& l = v.long_short.front();
        w.set_optional_text("LongIndex", l.long_leg.long_index);
        w.set_optional_text("LongPayTenor", l.long_leg.long_pay_tenor);
        w.set_optional_text("ShortIndex", l.short_leg.short_index);
        w.set_optional_text("ShortPayTenor", l.short_leg.short_pay_tenor);
        w.set_optional_flag("SpreadOnShort", l.short_leg.spread_on_short);
        w.set_optional_flag("IncludeSpread", l.coupon.include_spread);
        put_enum(
            w, "SubPeriodsCouponType", l.coupon.sub_periods_coupon_type, "sub-periods coupon type");
    }
}

namespace conventions_family {

void pack_tenor_basis_swap(const record_reader& root,
                           refdata::service::conventions_bundle& bundle) {
    // A tenor basis swap is one central convention row plus the parts the
    // document's form populated, and the three are three families.
    int conventions_seen = 0;
    int pay_receives_seen = 0;
    int long_shorts_seen = 0;
    for (const auto& x : root.children("TenorBasisSwap")) {
        auto item = conventions_mapper::map_tenor_basis_swap(x);
        if (item.convention)
            pack_row(bundle, "convention", conventions_seen++, *item.convention);
        for (const auto& part : item.pay_receive)
            pack_row(bundle, "tenor_basis_swap_pay_receive", pay_receives_seen++, part);
        for (const auto& part : item.long_short)
            pack_row(bundle, "tenor_basis_swap_long_short", long_shorts_seen++, part);
    }
}

void join_tenor_basis_swap(const std::vector<const package_row*>& rows, record_writer& root) {
    std::vector<refdata::domain::convention> central;
    std::vector<refdata::domain::tenor_basis_swap_pay_receive> pay_receives;
    std::vector<refdata::domain::tenor_basis_swap_long_short> long_shorts;

    for (const auto* row : rows) {
        // The one table carries every kind, and only this kind's rows have a
        // document element. A row of another kind is a convention with no
        // element to be written as, so it is dropped rather than written as a
        // tenor basis swap that carries no legs.
        if (row->family == "convention") {
            auto c = unpack_row<refdata::domain::convention>(*row);
            if (c.identity.kind_code == tenor_basis_swap_kind)
                central.push_back(std::move(c));
        } else if (row->family == "tenor_basis_swap_pay_receive") {
            pay_receives.push_back(unpack_row<refdata::domain::tenor_basis_swap_pay_receive>(*row));
        } else if (row->family == "tenor_basis_swap_long_short") {
            long_shorts.push_back(unpack_row<refdata::domain::tenor_basis_swap_long_short>(*row));
        } else {
            throw std::runtime_error(
                std::format("Family \"{}\" is not a tenor basis swap family.", row->family));
        }
    }

    // The three families are one document element, so a central row takes back
    // the parts that name it.
    for (const auto& c : central) {
        refdata::messaging::tenor_basis_swap_convention_item item;
        item.key.id = c.identity.id;
        item.convention = c;
        for (const auto& part : pay_receives)
            if (part.identity.convention_id == c.identity.id)
                item.pay_receive.push_back(part);
        for (const auto& part : long_shorts)
            if (part.identity.convention_id == c.identity.id)
                item.long_short.push_back(part);
        conventions_mapper::reverse_tenor_basis_swap(item, root.append("TenorBasisSwap"));
    }
}

}

}
