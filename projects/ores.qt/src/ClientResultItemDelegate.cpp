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
#include "ores.qt/ClientResultItemDelegate.hpp"
#include "ores.qt/ClientResultModel.hpp"
#include "ores.qt/DelegatePaintUtils.hpp"
#include "ores.qt/FontUtils.hpp"

#include <QPainter>
#include <QApplication>
#include <QStyleOptionViewItem>

namespace ores::qt {

using Column = ClientResultModel::Column;

namespace {

// State badge colors
const QColor state_done_bg(34, 197, 94);        // Green — Done
const QColor state_running_bg(234, 179, 8);     // Amber — Running
const QColor state_default_bg(107, 114, 128);   // Gray  — Inactive/Unsent
const QColor state_text(255, 255, 255);

// Outcome badge colors
const QColor outcome_success_bg(34, 197, 94);   // Green — Success
const QColor outcome_failed_bg(239, 68, 68);    // Red   — Failed / No Reply
const QColor outcome_default_bg(107, 114, 128); // Gray  — Pending
const QColor outcome_text(255, 255, 255);

struct badge_spec {
    QColor bg;
    QColor fg;
    QString text;
};

badge_spec state_badge(const QString& value) {
    if (value == QObject::tr("Done"))
        return {state_done_bg,    state_text, value};
    if (value == QObject::tr("Running"))
        return {state_running_bg, state_text, value};
    return {state_default_bg, state_text, value};
}

badge_spec outcome_badge(const QString& value) {
    if (value == QObject::tr("Success"))
        return {outcome_success_bg, outcome_text, value};
    if (value == QObject::tr("Failed") || value == QObject::tr("No Reply"))
        return {outcome_failed_bg,  outcome_text, value};
    return {outcome_default_bg, outcome_text, value};
}

} // namespace

ClientResultItemDelegate::ClientResultItemDelegate(QObject* parent)
    : QStyledItemDelegate(parent) {
    monospaceFont_ = FontUtils::monospace();
}

void ClientResultItemDelegate::paint(QPainter* painter,
    const QStyleOptionViewItem& option, const QModelIndex& index) const {

    QStyleOptionViewItem opt = option;
    initStyleOption(&opt, index);

    if (index.column() == Column::ServerState ||
        index.column() == Column::Outcome) {

        QApplication::style()->drawPrimitive(
            QStyle::PE_PanelItemViewItem, &opt, painter);

        const QString text = index.data(Qt::DisplayRole).toString();
        const auto spec = (index.column() == Column::ServerState)
            ? state_badge(text) : outcome_badge(text);

        QFont badgeFont = opt.font;
        badgeFont.setPointSize(qRound(badgeFont.pointSize() * 0.8));
        badgeFont.setBold(true);

        DelegatePaintUtils::draw_centered_badge(painter, opt.rect,
            spec.text, spec.bg, spec.fg, badgeFont);
        return;
    }

    DelegatePaintUtils::apply_foreground_role(opt, index);

    switch (index.column()) {
    case Column::WorkunitId:
    case Column::HostId:
        opt.font = monospaceFont_;
        opt.displayAlignment = Qt::AlignLeft | Qt::AlignVCenter;
        break;
    case Column::Version:
        opt.font = monospaceFont_;
        opt.displayAlignment = Qt::AlignCenter;
        break;
    case Column::ReceivedAt:
        opt.font = monospaceFont_;
        opt.displayAlignment = Qt::AlignLeft | Qt::AlignVCenter;
        break;
    default:
        break;
    }

    QApplication::style()->drawControl(QStyle::CE_ItemViewItem, &opt, painter);
}

QSize ClientResultItemDelegate::sizeHint(const QStyleOptionViewItem& option,
    const QModelIndex& index) const {

    QSize size = QStyledItemDelegate::sizeHint(option, index);
    if (index.column() == Column::ServerState ||
        index.column() == Column::Outcome) {
        size.setHeight(qMax(size.height(), 24));
        size.setWidth(qMax(size.width(), 70));
    }
    return size;
}

}
