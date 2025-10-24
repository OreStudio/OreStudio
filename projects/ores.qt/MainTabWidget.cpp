/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2024 Marco Craveiro <marco.craveiro@gmail.com>
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
#include <QPainter>
#include "ores.qt/CurrencyTabPage.hpp"
#include "ores.qt/MainTabWidget.hpp"

namespace ores::qt {

MainTabWidget::MainTabWidget(QWidget* parent)
    : QTabWidget(parent), currenciesIndex_(-1) {
    setTabsClosable(true);
    connect(this, SIGNAL(tabCloseRequested(int)), this, SLOT(closeTab(int)));
}

void MainTabWidget::openCurrencyTabPage() {
    if (currenciesIndex_ != -1) {
        setCurrentIndex(currenciesIndex_);
    } else {
        currenciesIndex_ = addTab(new CurrencyTabPage(client_), "Currencies");
    }
}

void MainTabWidget::closeTab(const int& index)
{
    if (index == -1) {
        return;
    }

    QWidget* tabItem = widget(index);
    removeTab(index);
    delete(tabItem);
    tabItem = nullptr;

    if (currenciesIndex_ == index) {
        currenciesIndex_ = -1;
    }
}

void MainTabWidget::paintEvent(QPaintEvent* e ) {
    QTabWidget::paintEvent(e);
    QPainter painter(this);
    if (count() == 0) {
        painter.drawText( rect(), Qt::AlignCenter, "EMPTY");
    }
}

}
