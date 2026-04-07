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
#ifndef ORES_QT_INSTRUMENT_FORM_REGISTRY_HPP
#define ORES_QT_INSTRUMENT_FORM_REGISTRY_HPP

#include <functional>
#include <map>
#include <vector>
#include <QString>
#include <QWidget>
#include "ores.trading.api/domain/product_type.hpp"

namespace ores::qt {

class IInstrumentForm;

/**
 * @brief Maps a @c product_type to its instrument form widget factory.
 *
 * Each registration carries the family's display name (used as the page
 * label in @c QStackedWidget bookkeeping and any future tab pickers) and
 * a factory closure that constructs a fresh form widget under the supplied
 * parent.
 *
 * Adding a ninth instrument family is one new @c IInstrumentForm subclass
 * plus one extra @ref registerForm call in @ref register_default_forms.
 * No file in @c TradeDetailDialog needs to change.
 */
class InstrumentFormRegistry final {
public:
    using product_type = ores::trading::domain::product_type;
    using Factory = std::function<IInstrumentForm*(QWidget* parent)>;

    void registerForm(product_type pt, QString displayName, Factory factory);

    /// True when at least one form is registered for @p pt.
    [[nodiscard]] bool contains(product_type pt) const noexcept;

    /// Construct a new form for @p pt parented at @p parent. Returns null
    /// if no factory has been registered for that family.
    [[nodiscard]] IInstrumentForm* createForm(
        product_type pt, QWidget* parent) const;

    /// Display name registered alongside the factory (for tab labels etc).
    [[nodiscard]] QString displayName(product_type pt) const;

    /// All registered families, in registration order.
    [[nodiscard]] std::vector<product_type> registeredTypes() const;

private:
    struct Entry {
        QString displayName;
        Factory factory;
    };
    std::vector<product_type> order_;
    std::map<product_type, Entry> entries_;
};

/**
 * @brief Register every shipping instrument form on @p registry.
 *
 * Called once from @c TradeDetailDialog construction. Implemented in
 * @c register_default_forms.cpp so that adding a new family only touches
 * a single registration callsite.
 */
void register_default_forms(InstrumentFormRegistry& registry);

}

#endif
