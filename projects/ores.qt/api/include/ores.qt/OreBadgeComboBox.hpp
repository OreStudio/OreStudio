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
#ifndef ORES_QT_ORE_BADGE_COMBO_BOX_HPP
#define ORES_QT_ORE_BADGE_COMBO_BOX_HPP

#include "ores.qt/export.hpp"
#include <QComboBox>
#include <string>

namespace ores::qt {

class BadgeCache;

/**
 * @brief QComboBox subclass whose closed-box display paints the current
 * selection as a badge pill, not just plain text.
 *
 * BadgeComboHelper::apply_combo_badges() already installs an item
 * delegate that paints popup rows as badges via
 * DelegatePaintUtils::draw_inline_badge() — but QComboBox does not use
 * the item delegate to paint its own closed-box display, so without this
 * class the popup shows badges while the box itself shows plain text.
 * Acts as a typed marker (like OreCurrencyComboBox) so uic promotes to
 * this class; apply_combo_badges() detects the marker and additionally
 * calls setBadgeSource() so the closed box matches the popup.
 */
class ORES_QT_API OreBadgeComboBox : public QComboBox {
    Q_OBJECT

public:
    explicit OreBadgeComboBox(QWidget* parent = nullptr);

    /**
     * @brief Set the badge cache and code_domain used to resolve the
     * closed-box badge colour for the current selection.
     */
    void setBadgeSource(BadgeCache* cache, std::string badge_key);

protected:
    void paintEvent(QPaintEvent* event) override;

private:
    BadgeCache* cache_ = nullptr;
    std::string badge_key_;
};

}

#endif
