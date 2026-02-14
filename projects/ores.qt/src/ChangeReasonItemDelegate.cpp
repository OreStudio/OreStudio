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
#include "ores.qt/ChangeReasonItemDelegate.hpp"
#include "ores.qt/ClientChangeReasonModel.hpp"
#include "ores.qt/DelegatePaintUtils.hpp"
#include "ores.qt/FontUtils.hpp"

#include <QPainter>
#include <QApplication>
#include <QStyleOptionViewItem>

namespace ores::qt {

namespace {

// Badge colors for boolean columns
const QColor yes_badge_bg(34, 197, 94);        // Green (#22c55e)
const QColor yes_badge_text(255, 255, 255);    // White
const QColor no_badge_bg(107, 114, 128);       // Gray (#6b7280)
const QColor no_badge_text(255, 255, 255);     // White

}

ChangeReasonItemDelegate::ChangeReasonItemDelegate(QObject* parent)
    : QStyledItemDelegate(parent) {
    monospaceFont_ = FontUtils::monospace();

    badgeFont_ = QFont();
    badgeFont_.setPointSize(7);
    badgeFont_.setBold(true);
}

void ChangeReasonItemDelegate::paint(QPainter* painter,
    const QStyleOptionViewItem& option, const QModelIndex& index) const {
    QStyleOptionViewItem opt = option;
    initStyleOption(&opt, index);

    // Handle badge columns (AppliesToAmend, AppliesToDelete, RequiresCommentary)
    if (index.column() == ClientChangeReasonModel::AppliesToAmend ||
        index.column() == ClientChangeReasonModel::AppliesToDelete ||
        index.column() == ClientChangeReasonModel::RequiresCommentary) {

        QStyle* style = QApplication::style();
        style->drawPrimitive(QStyle::PE_PanelItemViewItem, &opt, painter);

        QString text = index.data(Qt::DisplayRole).toString();
        QColor bgColor, textColor;
        QString badgeText;

        if (text == tr("Yes")) {
            bgColor = yes_badge_bg;
            textColor = yes_badge_text;
            badgeText = tr("Yes");
        } else {
            bgColor = no_badge_bg;
            textColor = no_badge_text;
            badgeText = tr("No");
        }

        DelegatePaintUtils::draw_centered_badge(painter, opt.rect,
            badgeText, bgColor, textColor, badgeFont_);
        return;
    }

    DelegatePaintUtils::apply_foreground_role(opt, index);

    // Apply formatting based on column
    switch (index.column()) {
        case ClientChangeReasonModel::Code:
            opt.displayAlignment = Qt::AlignLeft | Qt::AlignVCenter;
            break;
        case ClientChangeReasonModel::CategoryCode:
            opt.displayAlignment = Qt::AlignLeft | Qt::AlignVCenter;
            break;
        case ClientChangeReasonModel::DisplayOrder:
            opt.font = monospaceFont_;
            opt.displayAlignment = Qt::AlignCenter;
            break;
        case ClientChangeReasonModel::Version:
            opt.font = monospaceFont_;
            opt.displayAlignment = Qt::AlignCenter;
            break;
        case ClientChangeReasonModel::ModifiedBy:
            opt.displayAlignment = Qt::AlignLeft | Qt::AlignVCenter;
            break;
        case ClientChangeReasonModel::RecordedAt:
            opt.displayAlignment = Qt::AlignLeft | Qt::AlignVCenter;
            break;
        default:
            break;
    }

    QApplication::style()->drawControl(QStyle::CE_ItemViewItem, &opt, painter);
}

QSize ChangeReasonItemDelegate::sizeHint(const QStyleOptionViewItem& option,
                                          const QModelIndex& index) const {
    QSize size = QStyledItemDelegate::sizeHint(option, index);

    if (index.column() == ClientChangeReasonModel::AppliesToAmend ||
        index.column() == ClientChangeReasonModel::AppliesToDelete ||
        index.column() == ClientChangeReasonModel::RequiresCommentary) {
        size.setHeight(qMax(size.height(), 24));
        size.setWidth(qMax(size.width(), 50));
    }

    return size;
}

}
