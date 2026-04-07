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
#include "ores.qt/BadgeCache.hpp"
#include "ores.qt/ClientAccountModel.hpp"
#include "ores.qt/DelegatePaintUtils.hpp"
#include "ores.qt/FontUtils.hpp"

#include <QPainter>
#include <QApplication>
#include <QStyleOptionViewItem>

namespace ores::qt {

using Column = ClientAccountModel::Column;

AccountItemDelegate::AccountItemDelegate(BadgeCache* badgeCache, QObject* parent)
    : QStyledItemDelegate(parent),
      badgeCache_(badgeCache) {
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

        static const QColor default_gray(107, 114, 128);
        static const QColor white(255, 255, 255);

        QString text = index.data(Qt::DisplayRole).toString();
        QColor bgColor = default_gray;
        QColor textColor = white;
        QString badgeText;

        if (index.column() == Column::Status) {
            if (text == "Online") badgeText = tr("Online");
            else if (text == "Recent") badgeText = tr("Recent");
            else if (text == "Old") badgeText = tr("Old");
            else badgeText = tr("Never");
            if (badgeCache_) {
                auto* def = badgeCache_->resolve("login_status", text.toStdString());
                if (def) {
                    bgColor = QColor(QString::fromStdString(def->background_colour));
                    textColor = QColor(QString::fromStdString(def->text_colour));
                }
            }
        } else if (index.column() == Column::Locked) {
            badgeText = (text == "Locked") ? tr("Yes") : tr("No");
            if (badgeCache_) {
                auto* def = badgeCache_->resolve("account_locked", text.toStdString());
                if (def) {
                    bgColor = QColor(QString::fromStdString(def->background_colour));
                    textColor = QColor(QString::fromStdString(def->text_colour));
                }
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
