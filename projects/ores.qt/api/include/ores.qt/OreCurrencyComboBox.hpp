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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51 Franklin
 * Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#ifndef ORES_QT_ORE_CURRENCY_COMBO_BOX_HPP
#define ORES_QT_ORE_CURRENCY_COMBO_BOX_HPP

#include "ores.qt/export.hpp"
#include <QComboBox>

namespace ores::qt {

/**
 * @brief QComboBox subclass for ISO currency selection with flag icons.
 *
 * Acts as a typed marker so uic generates the right widget class and
 * includes the correct header. Populate with setup_currency_combo()
 * (FlagIconHelper.hpp), which fetches currency codes asynchronously and
 * applies flag icons.
 */
class ORES_QT_API OreCurrencyComboBox : public QComboBox {
public:
    explicit OreCurrencyComboBox(QWidget* parent = nullptr);
};

}

#endif
