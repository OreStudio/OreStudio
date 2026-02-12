/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#include "ores.qt/FeatureFlagItemDelegate.hpp"
#include "ores.qt/ClientFeatureFlagModel.hpp"
#include "ores.qt/DelegatePaintUtils.hpp"
#include "ores.qt/FontUtils.hpp"

#include <QPainter>
#include <QApplication>
#include <QStyleOptionViewItem>

namespace ores::qt {

namespace {

const QColor enabled_badge_bg(34, 197, 94);       // Green
const QColor enabled_badge_text(255, 255, 255);
const QColor disabled_badge_bg(107, 114, 128);    // Gray
const QColor disabled_badge_text(255, 255, 255);

}

FeatureFlagItemDelegate::FeatureFlagItemDelegate(QObject* parent)
    : QStyledItemDelegate(parent) {
    monospaceFont_ = FontUtils::monospace();

    badgeFont_ = QFont();
    badgeFont_.setPointSize(7);
    badgeFont_.setBold(true);
}

void FeatureFlagItemDelegate::paint(QPainter* painter, const QStyleOptionViewItem& option,
                                    const QModelIndex& index) const {
    QStyleOptionViewItem opt = option;
    initStyleOption(&opt, index);

    // Handle badge column for Enabled
    if (index.column() == ClientFeatureFlagModel::Enabled) {
        QStyle* style = QApplication::style();
        style->drawPrimitive(QStyle::PE_PanelItemViewItem, &opt, painter);

        QString text = index.data(Qt::DisplayRole).toString();
        QColor bgColor, textColor;
        QString badgeText;

        if (text == "Yes") {
            bgColor = enabled_badge_bg;
            textColor = enabled_badge_text;
            badgeText = tr("Yes");
        } else {
            bgColor = disabled_badge_bg;
            textColor = disabled_badge_text;
            badgeText = tr("No");
        }

        DelegatePaintUtils::draw_centered_badge(painter, opt.rect,
            badgeText, bgColor, textColor, badgeFont_);
        return;
    }

    DelegatePaintUtils::apply_foreground_role(opt, index);

    // Apply formatting based on column
    switch (index.column()) {
        case ClientFeatureFlagModel::Name:
            opt.displayAlignment = Qt::AlignLeft | Qt::AlignVCenter;
            break;
        case ClientFeatureFlagModel::RecordedBy:
            opt.displayAlignment = Qt::AlignLeft | Qt::AlignVCenter;
            break;
        case ClientFeatureFlagModel::RecordedAt:
            opt.font = monospaceFont_;
            opt.displayAlignment = Qt::AlignLeft | Qt::AlignVCenter;
            break;
        default:
            break;
    }

    QApplication::style()->drawControl(QStyle::CE_ItemViewItem, &opt, painter);
}

QSize FeatureFlagItemDelegate::sizeHint(const QStyleOptionViewItem& option,
                                        const QModelIndex& index) const {
    QSize size = QStyledItemDelegate::sizeHint(option, index);

    if (index.column() == ClientFeatureFlagModel::Enabled) {
        size.setHeight(qMax(size.height(), 24));
        size.setWidth(qMax(size.width(), 50));
    }

    return size;
}

}
