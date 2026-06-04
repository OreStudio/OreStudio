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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51 Franklin
 * Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#include "ores.qt/WorkflowStepLogWidget.hpp"
#include "ores.qt/ColorConstants.hpp"
#include "ores.qt/DelegatePaintUtils.hpp"
#include <QApplication>
#include <QHeaderView>
#include <QPainter>
#include <QStyleOptionViewItem>
#include <QStyledItemDelegate>
#include <QVBoxLayout>

namespace ores::qt {

namespace wfev = ores::workflow::messaging;

namespace {

enum ItemRole {
    BadgeTagRole = Qt::UserRole,
    BadgeColorRole = Qt::UserRole + 1,
};

enum class Col { Level = 0, Message, Context, Count };

class LevelBadgeDelegate final : public QStyledItemDelegate {
public:
    explicit LevelBadgeDelegate(QObject* parent = nullptr)
        : QStyledItemDelegate(parent) {}

    void paint(QPainter* painter,
               const QStyleOptionViewItem& option,
               const QModelIndex& index) const override {
        if (index.data(BadgeTagRole).toString() != QStringLiteral("badge")) {
            QStyledItemDelegate::paint(painter, option, index);
            return;
        }
        QStyleOptionViewItem opt = option;
        initStyleOption(&opt, index);
        QApplication::style()->drawPrimitive(QStyle::PE_PanelItemViewItem, &opt, painter);

        const QString text = index.data(Qt::DisplayRole).toString();
        const QColor bg = index.data(BadgeColorRole).value<QColor>();
        const QColor fg = color_constants::level_text;

        QFont badgeFont = opt.font;
        badgeFont.setPointSize(qRound(badgeFont.pointSize() * 0.8));
        badgeFont.setBold(true);
        DelegatePaintUtils::draw_centered_badge(painter, opt.rect, text, bg, fg, badgeFont);
    }

    QSize sizeHint(const QStyleOptionViewItem& option, const QModelIndex& index) const override {
        QSize s = QStyledItemDelegate::sizeHint(option, index);
        if (index.data(BadgeTagRole).toString() == QStringLiteral("badge"))
            s = QSize(qMax(s.width(), 70), qMax(s.height(), 24));
        return s;
    }
};

QTableWidgetItem* make_badge_item(const QString& text, const QColor& bg) {
    auto* item = new QTableWidgetItem(text);
    item->setData(BadgeTagRole, QStringLiteral("badge"));
    item->setData(BadgeColorRole, bg);
    item->setFlags(item->flags() & ~Qt::ItemIsEditable);
    return item;
}

QTableWidgetItem* make_item(const QString& text) {
    auto* item = new QTableWidgetItem(text);
    item->setTextAlignment(Qt::AlignVCenter | Qt::AlignLeft);
    item->setFlags(item->flags() & ~Qt::ItemIsEditable);
    return item;
}

QColor level_color(wfev::step_log_level level) {
    switch (level) {
        case wfev::step_log_level::info:
            return color_constants::level_info;
        case wfev::step_log_level::warn:
            return color_constants::level_warn;
        case wfev::step_log_level::error:
            return color_constants::level_error;
    }
    return color_constants::level_debug;
}

QString level_label(wfev::step_log_level level) {
    switch (level) {
        case wfev::step_log_level::info:
            return QStringLiteral("info");
        case wfev::step_log_level::warn:
            return QStringLiteral("warn");
        case wfev::step_log_level::error:
            return QStringLiteral("error");
    }
    return QStringLiteral("?");
}

} // namespace

WorkflowStepLogWidget::WorkflowStepLogWidget(QWidget* parent)
    : QWidget(parent)
    , headerLabel_(nullptr)
    , logTable_(nullptr) {
    setupUi();
}

void WorkflowStepLogWidget::setupUi() {
    auto* layout = new QVBoxLayout(this);
    layout->setContentsMargins(0, 4, 0, 0);

    headerLabel_ = new QLabel(tr("Step log — select a step above to view its log"), this);
    layout->addWidget(headerLabel_);

    logTable_ = new QTableWidget(0, static_cast<int>(Col::Count), this);
    logTable_->setHorizontalHeaderLabels({tr("Level"), tr("Message"), tr("Context")});
    logTable_->horizontalHeader()->setSectionResizeMode(static_cast<int>(Col::Level),
                                                        QHeaderView::ResizeToContents);
    logTable_->horizontalHeader()->setSectionResizeMode(static_cast<int>(Col::Message),
                                                        QHeaderView::Stretch);
    logTable_->horizontalHeader()->setSectionResizeMode(static_cast<int>(Col::Context),
                                                        QHeaderView::ResizeToContents);
    logTable_->setSelectionBehavior(QAbstractItemView::SelectRows);
    logTable_->setEditTriggers(QAbstractItemView::NoEditTriggers);
    logTable_->setAlternatingRowColors(true);
    logTable_->setItemDelegate(new LevelBadgeDelegate(logTable_));
    layout->addWidget(logTable_);
}

void WorkflowStepLogWidget::showLog(const QString& stepName,
                                    const std::vector<wfev::step_log_entry>& entries) {

    if (entries.empty()) {
        headerLabel_->setText(tr("Step log — %1 (no log entries)").arg(stepName));
        logTable_->setRowCount(0);
        return;
    }

    int warn_count = 0;
    int error_count = 0;
    for (const auto& e : entries) {
        if (e.level == wfev::step_log_level::warn)
            ++warn_count;
        if (e.level == wfev::step_log_level::error)
            ++error_count;
    }

    QString summary;
    if (error_count > 0 && warn_count > 0)
        summary = tr("%1 error(s), %2 warning(s)").arg(error_count).arg(warn_count);
    else if (error_count > 0)
        summary = tr("%1 error(s)").arg(error_count);
    else if (warn_count > 0)
        summary = tr("%1 warning(s)").arg(warn_count);
    else
        summary = tr("%1 info entr%2")
                      .arg(static_cast<int>(entries.size()))
                      .arg(entries.size() == 1 ? QStringLiteral("y") : QStringLiteral("ies"));

    headerLabel_->setText(tr("Step log — %1 (%2)").arg(stepName).arg(summary));

    logTable_->setRowCount(static_cast<int>(entries.size()));

    int row = 0;
    for (const auto& entry : entries) {
        logTable_->setItem(row,
                           static_cast<int>(Col::Level),
                           make_badge_item(level_label(entry.level), level_color(entry.level)));
        logTable_->setItem(
            row, static_cast<int>(Col::Message), make_item(QString::fromStdString(entry.message)));
        logTable_->setItem(row,
                           static_cast<int>(Col::Context),
                           make_item(entry.context.empty() ?
                                         QStringLiteral("—") :
                                         QString::fromStdString(entry.context)));
        ++row;
    }
}

void WorkflowStepLogWidget::clear() {
    headerLabel_->setText(tr("Step log — select a step above to view its log"));
    logTable_->setRowCount(0);
}

} // namespace ores::qt
