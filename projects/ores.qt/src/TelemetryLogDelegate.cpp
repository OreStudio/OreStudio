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

#include <QPainter>
#include <QApplication>
#include <QStyleOptionViewItem>

namespace ores::qt {

namespace {

// Level badge colors
const QColor level_trace_bg(107, 114, 128);     // Gray
const QColor level_trace_text(255, 255, 255);
const QColor level_debug_bg(59, 130, 246);      // Blue
const QColor level_debug_text(255, 255, 255);
const QColor level_info_bg(34, 197, 94);        // Green
const QColor level_info_text(255, 255, 255);
const QColor level_warn_bg(234, 179, 8);        // Amber
const QColor level_warn_text(255, 255, 255);
const QColor level_error_bg(239, 68, 68);       // Red
const QColor level_error_text(255, 255, 255);

}

TelemetryLogDelegate::TelemetryLogDelegate(QObject* parent)
    : QStyledItemDelegate(parent) {
    monospaceFont_ = QFont("Fira Code");
    monospaceFont_.setPointSize(10);

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
            bgColor = level_trace_bg;
            textColor = level_trace_text;
            badgeText = "TRACE";
        } else if (level == "debug") {
            bgColor = level_debug_bg;
            textColor = level_debug_text;
            badgeText = "DEBUG";
        } else if (level == "info") {
            bgColor = level_info_bg;
            textColor = level_info_text;
            badgeText = "INFO";
        } else if (level == "warn" || level == "warning") {
            bgColor = level_warn_bg;
            textColor = level_warn_text;
            badgeText = "WARN";
        } else if (level == "error") {
            bgColor = level_error_bg;
            textColor = level_error_text;
            badgeText = "ERROR";
        } else {
            bgColor = level_trace_bg;
            textColor = level_trace_text;
            badgeText = level.toUpper();
        }

        drawBadge(painter, opt.rect, badgeText, bgColor, textColor);
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

void TelemetryLogDelegate::drawBadge(QPainter* painter, const QRect& rect,
                                      const QString& text,
                                      const QColor& backgroundColor,
                                      const QColor& textColor) const {
    painter->save();
    painter->setRenderHint(QPainter::Antialiasing, true);

    QFontMetrics fm(badgeFont_);
    int textWidth = fm.horizontalAdvance(text);
    int padding = 5;
    int badgeWidth = textWidth + padding * 2;
    int badgeHeight = fm.height() + 2;

    int x = rect.center().x() - badgeWidth / 2;
    int y = rect.center().y() - badgeHeight / 2;
    QRect badgeRect(x, y, badgeWidth, badgeHeight);

    int radius = badgeHeight / 2;
    painter->setBrush(backgroundColor);
    painter->setPen(Qt::NoPen);
    painter->drawRoundedRect(badgeRect, radius, radius);

    painter->setFont(badgeFont_);
    painter->setPen(textColor);
    painter->drawText(badgeRect, Qt::AlignCenter, text);

    painter->restore();
}

}
