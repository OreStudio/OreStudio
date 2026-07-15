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
#include "ores.qt/ColorConstants.hpp"
#include "ores.qt/ColumnMetadata.hpp"
#include "ores.qt/EntityItemDelegate.hpp"
#include <QAbstractItemView>
#include <QColor>
#include <QGroupBox>
#include <QHeaderView>
#include <QListWidgetItem>
#include <QSplitter>
#include <QTableWidgetItem>
#include <QVBoxLayout>

namespace ores::qt {

using namespace ores::logging;

BadgeMappingBrowserWindow::BadgeMappingBrowserWindow(BadgeCache* badgeCache, QWidget* parent)
    : QWidget(parent)
    , badgeCache_(badgeCache)
    , domainList_(nullptr)
    , table_(nullptr)
    , badgeDelegate_(nullptr)
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

    auto* splitter = new QSplitter(Qt::Horizontal, this);

    auto* domainGroup = new QGroupBox(tr("Code Domains"), splitter);
    auto* domainLayout = new QVBoxLayout(domainGroup);
    domainList_ = new QListWidget(domainGroup);
    domainList_->setSelectionMode(QAbstractItemView::SingleSelection);
    domainList_->setAlternatingRowColors(true);
    connect(domainList_,
            &QListWidget::itemSelectionChanged,
            this,
            &BadgeMappingBrowserWindow::onDomainSelectionChanged);
    domainLayout->addWidget(domainList_);
    domainGroup->setMinimumWidth(220);
    domainGroup->setMaximumWidth(260);

    auto* tableGroup = new QGroupBox(tr("Mappings"), splitter);
    auto* tableLayout = new QVBoxLayout(tableGroup);
    table_ = new QTableWidget(tableGroup);
    table_->setColumnCount(2);
    table_->setHorizontalHeaderLabels({tr("Entity Code"), tr("Badge")});
    table_->horizontalHeader()->setStretchLastSection(true);
    table_->setEditTriggers(QAbstractItemView::NoEditTriggers);
    table_->setSelectionMode(QAbstractItemView::NoSelection);
    table_->verticalHeader()->setVisible(false);

    badgeDelegate_ =
        new EntityItemDelegate({column_style::text_left, column_style::badge_centered}, table_);
    table_->setItemDelegate(badgeDelegate_);

    tableLayout->addWidget(table_);

    splitter->addWidget(domainGroup);
    splitter->addWidget(tableGroup);
    splitter->setStretchFactor(0, 0);
    splitter->setStretchFactor(1, 1);

    layout->addWidget(splitter);

    emptyLabel_ = new QLabel(tr("Loading badge mappings…"), this);
    emptyLabel_->setAlignment(Qt::AlignCenter);
    layout->addWidget(emptyLabel_);
    emptyLabel_->setVisible(false);

    resize(700, 500);
    setWindowTitle(tr("Badge Mappings"));
}

void BadgeMappingBrowserWindow::reload() {
    if (!badgeCache_ || !badgeCache_->isLoaded()) {
        BOOST_LOG_SEV(lg(), debug) << "reload() called before badge cache is loaded; skipping";
        return;
    }
    populateDomainList();
}

void BadgeMappingBrowserWindow::populateDomainList() {
    const auto domains = badgeCache_->list_code_domains();

    const QString previous =
        domainList_->currentItem() ? domainList_->currentItem()->text() : QString();
    domainList_->blockSignals(true);
    domainList_->clear();
    for (const auto& d : domains)
        domainList_->addItem(QString::fromStdString(d));
    domainList_->blockSignals(false);

    if (domains.empty()) {
        table_->setRowCount(0);
        emptyLabel_->setText(tr("No badge mappings loaded."));
        emptyLabel_->setVisible(true);
        return;
    }

    emptyLabel_->setVisible(false);

    auto items = domainList_->findItems(previous, Qt::MatchExactly);
    domainList_->setCurrentItem(!items.isEmpty() ? items.first() : domainList_->item(0));
}

void BadgeMappingBrowserWindow::onDomainSelectionChanged() {
    auto* item = domainList_->currentItem();
    populateTable(item ? item->text() : QString());
}

void BadgeMappingBrowserWindow::populateTable(const QString& codeDomainCode) {
    table_->setRowCount(0);

    const std::string domain = codeDomainCode.toStdString();
    badgeDelegate_->set_badge_color_resolver(
        1, [cache = badgeCache_, domain](const QString& value) -> badge_color_pair {
            static const badge_color_pair fallback{color_constants::badge_fallback,
                                                   color_constants::badge_fallback_text};
            if (!cache)
                return fallback;
            const auto* def = cache->resolve(domain, value.toStdString());
            if (!def)
                return fallback;
            return {QColor(QString::fromStdString(def->background_colour)),
                    QColor(QString::fromStdString(def->text_colour))};
        });

    if (!badgeCache_ || codeDomainCode.isEmpty())
        return;

    const auto mappings = badgeCache_->list_by_domain(codeDomainCode.toStdString());
    table_->setRowCount(static_cast<int>(mappings.size()));

    int row = 0;
    for (const auto& [entityCode, def] : mappings) {
        auto* codeItem = new QTableWidgetItem(QString::fromStdString(entityCode));
        table_->setItem(row, 0, codeItem);

        // Text is the raw entity_code (e.g. "ACTIVE"), matching every
        // other badge column in the app -- the delegate's resolver keys
        // off this same display text, exactly like a real badge_key
        // column would. The badge_definition's own friendly label/
        // description go in the tooltip instead.
        auto* badgeItem = new QTableWidgetItem(QString::fromStdString(entityCode));
        badgeItem->setToolTip(QString::fromStdString(def->name + " — " + def->description));
        table_->setItem(row, 1, badgeItem);

        ++row;
    }

    BOOST_LOG_SEV(lg(), debug) << "Populated " << mappings.size() << " mappings for domain "
                               << codeDomainCode.toStdString();
}

}
