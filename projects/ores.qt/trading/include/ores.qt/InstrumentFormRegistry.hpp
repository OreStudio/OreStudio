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

#include "ores.trading.api/domain/product_type.hpp"
#include <QString>
#include <QWidget>
#include <functional>
#include <map>
#include <vector>

namespace ores::qt {

class IInstrumentForm;

/**
 * @brief Maps instrument families and specific trade type codes to form
 *        widget factories.
 *
 * Two registration tiers:
 *  - Per product-family (product_type): one form handles all sub-types in
 *    that family (e.g. BondInstrumentForm covers all bond variants).
 *  - Per trade-type-code (QString): a dedicated form handles exactly one
 *    sub-type (e.g. FxVanillaOptionInstrumentForm for "FxOption").
 *
 * @c TradeDetailDialog looks up by trade-type-code first, falling back to
 * the family form when no type-specific registration exists.
 */
class InstrumentFormRegistry final {
public:
    using product_type = ores::trading::domain::product_type;
    using Factory = std::function<IInstrumentForm*(QWidget* parent)>;

    // --- per-family registration ---

    void registerForm(product_type pt, QString displayName, Factory factory);

    [[nodiscard]] bool contains(product_type pt) const noexcept;
    [[nodiscard]] IInstrumentForm* createForm(product_type pt, QWidget* parent) const;
    [[nodiscard]] QString displayName(product_type pt) const;
    [[nodiscard]] std::vector<product_type> registeredTypes() const;

    // --- per-trade-type-code registration ---

    /// Register a dedicated form for a single trade type code.
    void registerTypeForm(const QString& trade_type_code, Factory factory);

    [[nodiscard]] bool containsTypeForm(const QString& trade_type_code) const noexcept;
    [[nodiscard]] IInstrumentForm* createTypeForm(const QString& trade_type_code,
                                                  QWidget* parent) const;

    /// All registered type codes, in registration order.
    [[nodiscard]] std::vector<QString> registeredTypeCodes() const;

private:
    struct Entry {
        QString displayName;
        Factory factory;
    };
    std::vector<product_type> order_;
    std::map<product_type, Entry> entries_;

    struct TypeEntry {
        Factory factory;
    };
    std::vector<QString> typeOrder_;
    std::map<QString, TypeEntry> typeEntries_;
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
