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
#include "ores.qt/AccountItemDelegate.hpp"
#include "ores.qt/ClientAccountModel.hpp"
#include "ores.qt/DelegatePaintUtils.hpp"
#include "ores.qt/FontUtils.hpp"

#include <QPainter>
#include <QApplication>
#include <QStyleOptionViewItem>

namespace ores::qt {

using Column = ClientAccountModel::Column;

namespace {

// Login status badge colors
const QColor status_never_bg(107, 114, 128);    // Gray for never logged in
const QColor status_never_text(255, 255, 255);
const QColor status_old_bg(234, 179, 8);        // Amber/Yellow for long ago
const QColor status_old_text(255, 255, 255);
const QColor status_recent_bg(59, 130, 246);    // Blue for recent
const QColor status_recent_text(255, 255, 255);
const QColor status_online_bg(34, 197, 94);     // Green for online
const QColor status_online_text(255, 255, 255);

// Locked status badge colors
const QColor locked_badge_bg(239, 68, 68);      // Red background for locked
const QColor locked_badge_text(255, 255, 255);
const QColor unlocked_badge_bg(107, 114, 128);  // Gray background for not locked
const QColor unlocked_badge_text(255, 255, 255);

}

AccountItemDelegate::AccountItemDelegate(QObject* parent)
    : QStyledItemDelegate(parent) {
    monospaceFont_ = FontUtils::monospace();
}

void AccountItemDelegate::paint(QPainter* painter, const QStyleOptionViewItem& option,
                                const QModelIndex& index) const {
    QStyleOptionViewItem opt = option;
    initStyleOption(&opt, index);

    // Handle badge columns
    if (index.column() == Column::Status ||
        index.column() == Column::Locked) {

        QStyle* style = QApplication::style();
        style->drawPrimitive(QStyle::PE_PanelItemViewItem, &opt, painter);

        QString text = index.data(Qt::DisplayRole).toString();
        QColor bgColor, textColor;
        QString badgeText;

        if (index.column() == Column::Status) {
            if (text == "Online") {
                bgColor = status_online_bg;
                textColor = status_online_text;
                badgeText = tr("Online");
            } else if (text == "Recent") {
                bgColor = status_recent_bg;
                textColor = status_recent_text;
                badgeText = tr("Recent");
            } else if (text == "Old") {
                bgColor = status_old_bg;
                textColor = status_old_text;
                badgeText = tr("Old");
            } else {
                bgColor = status_never_bg;
                textColor = status_never_text;
                badgeText = tr("Never");
            }
        } else if (index.column() == Column::Locked) {
            if (text == "Locked") {
                bgColor = locked_badge_bg;
                textColor = locked_badge_text;
                badgeText = tr("Yes");
            } else {
                bgColor = unlocked_badge_bg;
                textColor = unlocked_badge_text;
                badgeText = tr("No");
            }
        }

        // Derive badge font from view font for proper high-DPI scaling
        QFont badgeFont = opt.font;
        badgeFont.setPointSize(qRound(badgeFont.pointSize() * 0.8));
        badgeFont.setBold(true);

        DelegatePaintUtils::draw_centered_badge(painter, opt.rect,
            badgeText, bgColor, textColor, badgeFont);
        return;
    }

    DelegatePaintUtils::apply_foreground_role(opt, index);

    // Apply formatting based on column
    switch (index.column()) {
        case Column::Username:
            opt.displayAlignment = Qt::AlignLeft | Qt::AlignVCenter;
            break;
        case Column::Email:
            opt.displayAlignment = Qt::AlignLeft | Qt::AlignVCenter;
            break;
        case Column::Version:
            opt.font = monospaceFont_;
            opt.displayAlignment = Qt::AlignCenter;
            break;
        case Column::ModifiedBy:
            opt.displayAlignment = Qt::AlignLeft | Qt::AlignVCenter;
            break;
        case Column::RecordedAt:
            opt.font = monospaceFont_;
            opt.displayAlignment = Qt::AlignLeft | Qt::AlignVCenter;
            break;
        default:
            break;
    }

    QApplication::style()->drawControl(QStyle::CE_ItemViewItem, &opt, painter);
}

QSize AccountItemDelegate::sizeHint(const QStyleOptionViewItem& option,
                                    const QModelIndex& index) const {
    QSize size = QStyledItemDelegate::sizeHint(option, index);

    if (index.column() == Column::Status ||
        index.column() == Column::Locked) {
        size.setHeight(qMax(size.height(), 24));
        if (index.column() == Column::Status) {
            size.setWidth(qMax(size.width(), 70));
        } else {
            size.setWidth(qMax(size.width(), 50));
        }
    }

    return size;
}

}
