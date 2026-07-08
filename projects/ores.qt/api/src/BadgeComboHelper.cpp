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
#include "ores.qt/BadgeComboHelper.hpp"
#include "ores.dq.api/domain/badge_definition.hpp"
#include "ores.qt/BadgeCache.hpp"
#include "ores.qt/ColorConstants.hpp"
#include "ores.qt/DelegatePaintUtils.hpp"
#include <QApplication>
#include <QPainter>
#include <QPointer>
#include <QStyledItemDelegate>

namespace ores::qt {

namespace {

class BadgeItemDelegate final : public QStyledItemDelegate {
public:
    BadgeItemDelegate(QObject* parent, QPointer<BadgeCache> cache, std::string badge_key)
        : QStyledItemDelegate(parent)
        , cache_(std::move(cache))
        , badge_key_(std::move(badge_key)) {}

    void paint(QPainter* painter,
               const QStyleOptionViewItem& option,
               const QModelIndex& index) const override {
        QStyleOptionViewItem opt = option;
        initStyleOption(&opt, index);
        opt.text.clear();
        QApplication::style()->drawControl(QStyle::CE_ItemViewItem, &opt, painter);

        const QString text = index.data(Qt::DisplayRole).toString();
        if (text.isEmpty())
            return;

        QColor bg = color_constants::badge_fallback;
        QColor fg = color_constants::badge_fallback_text;
        if (const auto* def = cache_ ? cache_->resolve(badge_key_, text.toStdString()) : nullptr) {
            bg = QColor(QString::fromStdString(def->background_colour));
            fg = QColor(QString::fromStdString(def->text_colour));
        }

        QFont badgeFont = opt.font;
        badgeFont.setPointSize(qRound(badgeFont.pointSize() * 0.8));
        badgeFont.setBold(true);

        QRect rect = opt.rect;
        DelegatePaintUtils::draw_inline_badge(painter, rect, text, bg, fg, badgeFont);
    }

private:
    QPointer<BadgeCache> cache_;
    std::string badge_key_;
};

}

void apply_combo_badges(QComboBox* combo, BadgeCache* cache, const std::string& badge_key) {
    if (!combo || !cache)
        return;

    combo->setItemDelegate(new BadgeItemDelegate(combo, cache, badge_key));
}

void setup_badge_combo(QObject* context,
                       QComboBox* combo,
                       BadgeCache* cache,
                       const std::string& badge_key) {
    (void)context;
    apply_combo_badges(combo, cache, badge_key);
}

}
