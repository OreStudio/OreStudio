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
#include "ores.qt/TelemetryLogDelegate.hpp"
#include "ores.qt/ClientTelemetryLogModel.hpp"
#include "ores.qt/DelegatePaintUtils.hpp"
#include "ores.qt/ColorConstants.hpp"
#include "ores.qt/FontUtils.hpp"

#include <QPainter>
#include <QApplication>
#include <QStyleOptionViewItem>

namespace ores::qt {

TelemetryLogDelegate::TelemetryLogDelegate(QObject* parent)
    : QStyledItemDelegate(parent) {
    monospaceFont_ = FontUtils::monospace();

    badgeFont_ = QFont();
    badgeFont_.setPointSize(7);
    badgeFont_.setBold(true);
}

void TelemetryLogDelegate::paint(QPainter* painter,
                                  const QStyleOptionViewItem& option,
                                  const QModelIndex& index) const {
    QStyleOptionViewItem opt = option;
    initStyleOption(&opt, index);

    // Handle Level column with badges
    if (index.column() == ClientTelemetryLogModel::Level) {
        QStyle* style = QApplication::style();
        style->drawPrimitive(QStyle::PE_PanelItemViewItem, &opt, painter);

        QString level = index.data(Qt::DisplayRole).toString().toLower();
        QColor bgColor, textColor;
        QString badgeText;

        if (level == "trace") {
            bgColor = color_constants::level_trace;
            textColor = color_constants::level_text;
            badgeText = "TRACE";
        } else if (level == "debug") {
            bgColor = color_constants::level_debug;
            textColor = color_constants::level_text;
            badgeText = "DEBUG";
        } else if (level == "info") {
            bgColor = color_constants::level_info;
            textColor = color_constants::level_text;
            badgeText = "INFO";
        } else if (level == "warn" || level == "warning") {
            bgColor = color_constants::level_warn;
            textColor = color_constants::level_text;
            badgeText = "WARN";
        } else if (level == "error") {
            bgColor = color_constants::level_error;
            textColor = color_constants::level_text;
            badgeText = "ERROR";
        } else {
            bgColor = color_constants::level_trace;
            textColor = color_constants::level_text;
            badgeText = level.toUpper();
        }

        DelegatePaintUtils::draw_centered_badge(painter, opt.rect,
            badgeText, bgColor, textColor, badgeFont_);
        return;
    }

    // Apply formatting based on column
    switch (index.column()) {
    case ClientTelemetryLogModel::Timestamp:
        opt.font = monospaceFont_;
        opt.displayAlignment = Qt::AlignLeft | Qt::AlignVCenter;
        break;
    case ClientTelemetryLogModel::Source:
        opt.font = monospaceFont_;
        opt.displayAlignment = Qt::AlignLeft | Qt::AlignVCenter;
        break;
    case ClientTelemetryLogModel::Component:
        opt.font = monospaceFont_;
        opt.displayAlignment = Qt::AlignLeft | Qt::AlignVCenter;
        break;
    case ClientTelemetryLogModel::Message:
        opt.displayAlignment = Qt::AlignLeft | Qt::AlignVCenter;
        break;
    default:
        break;
    }

    QApplication::style()->drawControl(QStyle::CE_ItemViewItem, &opt, painter);
}

QSize TelemetryLogDelegate::sizeHint(const QStyleOptionViewItem& option,
                                      const QModelIndex& index) const {
    QSize size = QStyledItemDelegate::sizeHint(option, index);

    if (index.column() == ClientTelemetryLogModel::Level) {
        size.setHeight(qMax(size.height(), 24));
        size.setWidth(qMax(size.width(), 60));
    }

    return size;
}

}
