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
#include "ores.qt/InstrumentFormRegistry.hpp"

#include "ores.qt/IInstrumentForm.hpp"
#include "ores.qt/BondInstrumentForm.hpp"
#include "ores.qt/FxInstrumentForm.hpp"
#include "ores.qt/SwapInstrumentForm.hpp"

namespace ores::qt {

void InstrumentFormRegistry::registerForm(
    product_type pt, QString displayName, Factory factory) {
    if (entries_.find(pt) == entries_.end()) {
        order_.push_back(pt);
    }
    entries_[pt] = Entry{std::move(displayName), std::move(factory)};
}

bool InstrumentFormRegistry::contains(product_type pt) const noexcept {
    return entries_.find(pt) != entries_.end();
}

IInstrumentForm* InstrumentFormRegistry::createForm(
    product_type pt, QWidget* parent) const {
    auto it = entries_.find(pt);
    if (it == entries_.end()) return nullptr;
    return it->second.factory(parent);
}

QString InstrumentFormRegistry::displayName(product_type pt) const {
    auto it = entries_.find(pt);
    if (it == entries_.end()) return {};
    return it->second.displayName;
}

std::vector<InstrumentFormRegistry::product_type>
InstrumentFormRegistry::registeredTypes() const {
    return order_;
}

void register_default_forms(InstrumentFormRegistry& registry) {
    // Each instrument family is registered here as it gets migrated from the
    // hardcoded TradeDetailDialog tabs into its own IInstrumentForm subclass.
    // Adding a ninth family is one new subclass plus one new line below.
    using PT = ores::trading::domain::product_type;

    registry.registerForm(PT::bond, QStringLiteral("Bond"),
        [](QWidget* parent) -> IInstrumentForm* {
            return new BondInstrumentForm(parent);
        });
    registry.registerForm(PT::swap, QStringLiteral("Swap"),
        [](QWidget* parent) -> IInstrumentForm* {
            return new SwapInstrumentForm(parent);
        });
    registry.registerForm(PT::fx, QStringLiteral("FX"),
        [](QWidget* parent) -> IInstrumentForm* {
            return new FxInstrumentForm(parent);
        });
}

}
