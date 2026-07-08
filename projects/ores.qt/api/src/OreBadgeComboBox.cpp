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
#include "ores.qt/OreBadgeComboBox.hpp"
#include "ores.qt/BadgeCache.hpp"
#include "ores.qt/ColorConstants.hpp"
#include "ores.qt/DelegatePaintUtils.hpp"
#include "ores.dq.api/domain/badge_definition.hpp"
#include <QStyleOptionComboBox>
#include <QStylePainter>

namespace ores::qt {

OreBadgeComboBox::OreBadgeComboBox(QWidget* parent)
    : QComboBox(parent) {}

void OreBadgeComboBox::setBadgeSource(BadgeCache* cache, std::string badge_key) {
    cache_ = cache;
    badge_key_ = std::move(badge_key);
    update();
}

void OreBadgeComboBox::paintEvent(QPaintEvent*) {
    QStylePainter painter(this);
    painter.setPen(palette().color(QPalette::Text));

    QStyleOptionComboBox opt;
    initStyleOption(&opt);
    const QString text = opt.currentText;
    opt.currentText.clear();
    painter.drawComplexControl(QStyle::CC_ComboBox, opt);

    if (text.isEmpty())
        return;

    QRect textRect = style()->subControlRect(QStyle::CC_ComboBox, &opt, QStyle::SC_ComboBoxEditField, this);

    QColor bg = color_constants::badge_fallback;
    QColor fg = color_constants::badge_fallback_text;
    if (const auto* def = cache_ ? cache_->resolve(badge_key_, text.toStdString()) : nullptr) {
        bg = QColor(QString::fromStdString(def->background_colour));
        fg = QColor(QString::fromStdString(def->text_colour));
    }

    QFont badgeFont = font();
    badgeFont.setPointSize(qRound(badgeFont.pointSize() * 0.8));
    badgeFont.setBold(true);

    DelegatePaintUtils::draw_inline_badge(&painter, textRect, text, bg, fg, badgeFont);
}

}
