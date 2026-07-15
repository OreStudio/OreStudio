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
#include "ores.qt/BadgeMappingBrowserWindow.hpp"
#include "ores.dq.api/domain/badge_definition.hpp"
#include "ores.qt/BadgeCache.hpp"
#include <QAbstractItemView>
#include <QColor>
#include <QHBoxLayout>
#include <QHeaderView>
#include <QTableWidgetItem>

namespace ores::qt {

using namespace ores::logging;

BadgeMappingBrowserWindow::BadgeMappingBrowserWindow(BadgeCache* badgeCache, QWidget* parent)
    : QWidget(parent)
    , badgeCache_(badgeCache)
    , domainPicker_(nullptr)
    , table_(nullptr)
    , emptyLabel_(nullptr) {
    setupUi();

    if (badgeCache_) {
        connect(badgeCache_, &BadgeCache::loaded, this, &BadgeMappingBrowserWindow::reload);
        if (badgeCache_->isLoaded())
            reload();
    }
}

void BadgeMappingBrowserWindow::setupUi() {
    auto* layout = new QVBoxLayout(this);

    auto* pickerRow = new QHBoxLayout();
    pickerRow->addWidget(new QLabel(tr("Code domain:"), this));
    domainPicker_ = new QComboBox(this);
    domainPicker_->setMinimumWidth(240);
    connect(domainPicker_,
            QOverload<int>::of(&QComboBox::currentIndexChanged),
            this,
            &BadgeMappingBrowserWindow::onDomainChanged);
    pickerRow->addWidget(domainPicker_);
    pickerRow->addStretch();
    layout->addLayout(pickerRow);

    table_ = new QTableWidget(this);
    table_->setColumnCount(2);
    table_->setHorizontalHeaderLabels({tr("Entity Code"), tr("Badge")});
    table_->horizontalHeader()->setStretchLastSection(true);
    table_->setEditTriggers(QAbstractItemView::NoEditTriggers);
    table_->setSelectionMode(QAbstractItemView::NoSelection);
    table_->verticalHeader()->setVisible(false);
    layout->addWidget(table_);

    emptyLabel_ = new QLabel(tr("Loading badge mappings…"), this);
    emptyLabel_->setAlignment(Qt::AlignCenter);
    layout->addWidget(emptyLabel_);
    emptyLabel_->setVisible(false);

    resize(600, 500);
    setWindowTitle(tr("Badge Mappings"));
}

void BadgeMappingBrowserWindow::reload() {
    if (!badgeCache_ || !badgeCache_->isLoaded()) {
        BOOST_LOG_SEV(lg(), debug) << "reload() called before badge cache is loaded; skipping";
        return;
    }
    populateDomainPicker();
}

void BadgeMappingBrowserWindow::populateDomainPicker() {
    const auto domains = badgeCache_->list_code_domains();

    const QString previous = domainPicker_->currentText();
    domainPicker_->blockSignals(true);
    domainPicker_->clear();
    for (const auto& d : domains)
        domainPicker_->addItem(QString::fromStdString(d));
    domainPicker_->blockSignals(false);

    if (domains.empty()) {
        table_->setVisible(false);
        emptyLabel_->setText(tr("No badge mappings loaded."));
        emptyLabel_->setVisible(true);
        return;
    }

    table_->setVisible(true);
    emptyLabel_->setVisible(false);

    int idx = domainPicker_->findText(previous);
    if (idx < 0)
        idx = 0;
    domainPicker_->setCurrentIndex(idx);
    populateTable(domainPicker_->itemText(idx));
}

void BadgeMappingBrowserWindow::onDomainChanged(int index) {
    if (index < 0)
        return;
    populateTable(domainPicker_->itemText(index));
}

void BadgeMappingBrowserWindow::populateTable(const QString& codeDomainCode) {
    table_->setRowCount(0);
    if (!badgeCache_ || codeDomainCode.isEmpty())
        return;

    const auto mappings = badgeCache_->list_by_domain(codeDomainCode.toStdString());
    table_->setRowCount(static_cast<int>(mappings.size()));

    int row = 0;
    for (const auto& [entityCode, def] : mappings) {
        auto* codeItem = new QTableWidgetItem(QString::fromStdString(entityCode));
        table_->setItem(row, 0, codeItem);

        auto* badgeItem = new QTableWidgetItem(QString::fromStdString(def->name));
        const QColor bg(QString::fromStdString(def->background_colour));
        const QColor fg(QString::fromStdString(def->text_colour));
        badgeItem->setBackground(bg);
        badgeItem->setForeground(fg);
        badgeItem->setToolTip(QString::fromStdString(def->description));
        table_->setItem(row, 1, badgeItem);

        ++row;
    }

    BOOST_LOG_SEV(lg(), debug) << "Populated " << mappings.size() << " mappings for domain "
                               << codeDomainCode.toStdString();
}

}
