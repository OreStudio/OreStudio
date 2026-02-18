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
#include "ores.qt/EntityItemDelegate.hpp"
#include "ores.qt/DelegatePaintUtils.hpp"
#include "ores.qt/FontUtils.hpp"

#include <QApplication>
#include <QStyleOptionViewItem>

namespace ores::qt {

EntityItemDelegate::EntityItemDelegate(std::vector<column_style> styles,
    QObject* parent)
    : QStyledItemDelegate(parent),
      styles_(std::move(styles)),
      monospaceFont_(FontUtils::monospace()) {
}

void EntityItemDelegate::paint(QPainter* painter,
    const QStyleOptionViewItem& option, const QModelIndex& index) const {
    QStyleOptionViewItem opt = option;
    initStyleOption(&opt, index);

    DelegatePaintUtils::apply_foreground_role(opt, index);

    const auto col = static_cast<std::size_t>(index.column());
    const auto style = col < styles_.size()
        ? styles_[col] : column_style::text_left;

    switch (style) {
    case column_style::icon_centered:
        DelegatePaintUtils::paint_centered_icon(painter, option, index);
        return;
    case column_style::badge_centered: {
        // Draw selection/hover background first
        opt.text.clear();
        QApplication::style()->drawControl(QStyle::CE_ItemViewItem, &opt, painter);

        const QString text = index.data(Qt::DisplayRole).toString();
        if (!text.isEmpty()) {
            badge_color_pair colors{QColor(0x6B, 0x72, 0x80), Qt::white};
            if (badge_resolver_)
                colors = badge_resolver_(text);

            QFont badgeFont = opt.font;
            badgeFont.setPointSize(qRound(badgeFont.pointSize() * 0.8));
            badgeFont.setBold(true);

            DelegatePaintUtils::draw_centered_badge(painter, opt.rect,
                text, colors.background, colors.foreground, badgeFont);
        }
        return;
    }
    case column_style::text_left:
        opt.displayAlignment = Qt::AlignLeft | Qt::AlignVCenter;
        break;
    case column_style::text_center:
        opt.displayAlignment = Qt::AlignCenter;
        break;
    case column_style::mono_left:
        opt.font = monospaceFont_;
        opt.displayAlignment = Qt::AlignLeft | Qt::AlignVCenter;
        break;
    case column_style::mono_center:
        opt.font = monospaceFont_;
        opt.displayAlignment = Qt::AlignCenter;
        break;
    case column_style::mono_right:
        opt.font = monospaceFont_;
        opt.displayAlignment = Qt::AlignRight | Qt::AlignVCenter;
        break;
    case column_style::mono_bold_left:
        opt.font = monospaceFont_;
        opt.font.setBold(true);
        opt.displayAlignment = Qt::AlignLeft | Qt::AlignVCenter;
        break;
    case column_style::mono_bold_center:
        opt.font = monospaceFont_;
        opt.font.setBold(true);
        opt.displayAlignment = Qt::AlignCenter;
        break;
    }

    QApplication::style()->drawControl(QStyle::CE_ItemViewItem, &opt, painter);
}

void EntityItemDelegate::set_badge_color_resolver(badge_color_resolver resolver) {
    badge_resolver_ = std::move(resolver);
}

}
