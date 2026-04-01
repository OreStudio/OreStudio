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
#include "ores.qt/PartyPickerDialog.hpp"

#include <algorithm>
#include <set>
#include <boost/uuid/uuid_io.hpp>
#include <boost/uuid/string_generator.hpp>
#include <QVBoxLayout>
#include <QHBoxLayout>
#include <QGroupBox>
#include <QFrame>
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/ImageCache.hpp"
#include "ores.qt/FlagIconHelper.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.qt/WidgetUtils.hpp"

namespace ores::qt {

PartyPickerDialog::PartyPickerDialog(
    const std::vector<PartyInfo>& parties,
    ClientManager* clientManager,
    ImageCache* imageCache,
    QWidget* parent)
    : QDialog(parent),
      clientManager_(clientManager),
      imageCache_(imageCache),
      parties_(parties) {

    setupUi();

    connect(okButton_,     &QPushButton::clicked,
            this,          &PartyPickerDialog::onOkClicked);
    connect(cancelButton_, &QPushButton::clicked,
            this,          &QDialog::reject);
    connect(listWidget_,   &QListWidget::itemDoubleClicked,
            this,          &PartyPickerDialog::onOkClicked);

    // Apply flag icons once the image cache has finished loading
    if (imageCache_) {
        connect(imageCache_, &ImageCache::allLoaded, this, [this]() {
            set_combo_flag_icons(centreCombo_, [this](const std::string& code) {
                return code.empty() ? QIcon{} : imageCache_->getBusinessCentreFlagIcon(code);
            });
            for (int i = 0; i < listWidget_->count(); ++i) {
                auto* item = listWidget_->item(i);
                const auto bc = item->data(Qt::UserRole + 1).toString().toStdString();
                if (!bc.empty())
                    item->setIcon(imageCache_->getBusinessCentreFlagIcon(bc));
            }
        });
    }
}

boost::uuids::uuid PartyPickerDialog::selectedPartyId() const {
    return selectedId_;
}

QString PartyPickerDialog::selectedPartyName() const {
    return selectedName_;
}

void PartyPickerDialog::setupUi() {
    WidgetUtils::setupComboBoxes(this);
    setWindowTitle("Select Party");
    setModal(true);
    setMinimumWidth(480);
    setFixedWidth(480);
    setSizeGripEnabled(false);

    auto* mainLayout = new QVBoxLayout(this);
    mainLayout->setSpacing(10);

    auto* infoLabel = new QLabel(
        "Your account is associated with multiple parties.\n"
        "Please select the party context for this session.", this);
    infoLabel->setWordWrap(true);
    mainLayout->addWidget(infoLabel);

    // ----------------------------------------------------------------
    // System party section
    // ----------------------------------------------------------------
    const PartyInfo* systemParty = nullptr;
    for (const auto& p : parties_) {
        if (p.is_system()) { systemParty = &p; break; }
    }

    systemSection_ = new QGroupBox(tr("System"), this);
    auto* sysLayout = new QHBoxLayout(systemSection_);
    sysLayout->setContentsMargins(8, 4, 8, 4);

    systemPartyLabel_ = new QLabel(
        systemParty ? systemParty->name : QString{}, systemSection_);
    systemPartyLabel_->setStyleSheet(
        "QLabel { font-weight: bold; color: #e8a000; }");

    auto* sysSelectBtn = new QPushButton(tr("Select"), systemSection_);
    sysSelectBtn->setMaximumWidth(80);
    sysSelectBtn->setIcon(
        IconUtils::createRecoloredIcon(Icon::Checkmark, IconUtils::DefaultIconColor));

    sysLayout->addWidget(systemPartyLabel_, 1);
    sysLayout->addWidget(sysSelectBtn);
    systemSection_->setVisible(systemParty != nullptr);
    mainLayout->addWidget(systemSection_);

    if (systemParty) {
        const PartyInfo* sp = systemParty;
        connect(sysSelectBtn, &QPushButton::clicked, this, [this, sp]() {
            selectSystemParty();
        });
        connect(systemPartyLabel_, &QLabel::linkActivated, this, [this](){
            selectSystemParty();
        });
    }

    // ----------------------------------------------------------------
    // Filter row
    // ----------------------------------------------------------------
    auto* filterRow = new QHBoxLayout();

    filterEdit_ = new QLineEdit(this);
    filterEdit_->setPlaceholderText(tr("Filter parties..."));
    filterEdit_->setClearButtonEnabled(true);
    filterRow->addWidget(filterEdit_, 1);

    centreCombo_ = new QComboBox(this);
    centreCombo_->setMinimumWidth(120);
    populateCentreCombo();
    filterRow->addWidget(centreCombo_);

    mainLayout->addLayout(filterRow);

    // ----------------------------------------------------------------
    // Operational party list
    // ----------------------------------------------------------------
    listWidget_ = new QListWidget(this);
    listWidget_->setAlternatingRowColors(true);
    listWidget_->setMinimumHeight(200);

    // Collect operational parties sorted by name
    std::vector<const PartyInfo*> ops;
    for (const auto& p : parties_) {
        if (!p.is_system()) ops.push_back(&p);
    }
    std::sort(ops.begin(), ops.end(),
        [](const PartyInfo* a, const PartyInfo* b) {
            return a->name.toLower() < b->name.toLower();
        });

    for (const auto* p : ops) {
        auto* item = new QListWidgetItem(listWidget_);
        const QString bc = p->business_center_code;
        item->setText(bc.isEmpty()
            ? p->name
            : QString("%1  %2").arg(p->name, bc));
        // UserRole: party name, UserRole+1: business centre code,
        // UserRole+2: party UUID as string
        item->setData(Qt::UserRole,     p->name);
        item->setData(Qt::UserRole + 1, bc);
        item->setData(Qt::UserRole + 2,
            QString::fromStdString(boost::uuids::to_string(p->id)));

        if (imageCache_ && !bc.isEmpty())
            item->setIcon(imageCache_->getBusinessCentreFlagIcon(bc.toStdString()));

        listWidget_->addItem(item);
    }

    mainLayout->addWidget(listWidget_);

    // ----------------------------------------------------------------
    // Buttons
    // ----------------------------------------------------------------
    okButton_     = new QPushButton(tr("Select"), this);
    cancelButton_ = new QPushButton(tr("Cancel"), this);
    okButton_->setEnabled(false);
    okButton_->setDefault(true);
    okButton_->setIcon(
        IconUtils::createRecoloredIcon(Icon::Checkmark, IconUtils::DefaultIconColor));
    cancelButton_->setIcon(
        IconUtils::createRecoloredIcon(Icon::Dismiss, IconUtils::DefaultIconColor));

    auto* btnLayout = new QHBoxLayout();
    btnLayout->addStretch();
    btnLayout->addWidget(cancelButton_);
    btnLayout->addWidget(okButton_);
    mainLayout->addSpacing(4);
    mainLayout->addLayout(btnLayout);

    // ----------------------------------------------------------------
    // Wire up filtering
    // ----------------------------------------------------------------
    connect(filterEdit_, &QLineEdit::textChanged,
            this, [this](const QString&) { applyFilter(); });
    connect(centreCombo_, QOverload<int>::of(&QComboBox::currentIndexChanged),
            this, [this](int) { applyFilter(); });

    connect(listWidget_, &QListWidget::itemClicked,
            this, [this](QListWidgetItem* item) {
        selectOperationalItem(item);
    });

    // Auto-select if only one operational party and no system party
    if (!systemParty && listWidget_->count() == 1) {
        listWidget_->setCurrentRow(0);
        selectOperationalItem(listWidget_->item(0));
    }

    // If system party exists, pre-select it
    if (systemParty) {
        selectSystemParty();
    } else if (!ops.empty()) {
        listWidget_->setCurrentRow(0);
        selectOperationalItem(listWidget_->item(0));
    }

    filterEdit_->setFocus();
}

void PartyPickerDialog::populateCentreCombo() {
    std::set<QString> codes;
    for (const auto& p : parties_) {
        if (!p.is_system() && !p.business_center_code.isEmpty())
            codes.insert(p.business_center_code);
    }

    centreCombo_->addItem(tr("All centres"), QString{});
    for (const auto& code : codes) {
        centreCombo_->addItem(code, code);
        if (imageCache_)
            centreCombo_->setItemIcon(centreCombo_->count() - 1,
                imageCache_->getBusinessCentreFlagIcon(code.toStdString()));
    }

    // Hide the combo if there's only one unique centre (or none)
    centreCombo_->setVisible(codes.size() > 1);
}

void PartyPickerDialog::applyFilter() {
    const QString text = filterEdit_->text().trimmed().toLower();
    const QString centre = centreCombo_->currentData().toString();

    for (int i = 0; i < listWidget_->count(); ++i) {
        auto* item = listWidget_->item(i);
        const QString name = item->data(Qt::UserRole).toString().toLower();
        const QString bc   = item->data(Qt::UserRole + 1).toString();

        const bool nameMatch   = text.isEmpty() || name.contains(text);
        const bool centreMatch = centre.isEmpty() || bc == centre;
        item->setHidden(!(nameMatch && centreMatch));
    }
}

void PartyPickerDialog::selectSystemParty() {
    for (const auto& p : parties_) {
        if (!p.is_system()) continue;
        selectedId_   = p.id;
        selectedName_ = p.name;
        listWidget_->clearSelection();
        systemSection_->setStyleSheet(
            "QGroupBox { border: 1px solid #e8a000; border-radius: 4px; }");
        okButton_->setEnabled(true);
        return;
    }
}

void PartyPickerDialog::selectOperationalItem(QListWidgetItem* item) {
    if (!item) return;
    selectedName_ = item->data(Qt::UserRole).toString();
    try {
        boost::uuids::string_generator gen;
        selectedId_ = gen(item->data(Qt::UserRole + 2).toString().toStdString());
    } catch (...) {
        selectedId_ = boost::uuids::uuid{};
    }
    systemSection_->setStyleSheet(QString{});
    okButton_->setEnabled(true);
}

void PartyPickerDialog::onOkClicked() {
    if (!clientManager_) {
        MessageBoxHelper::critical(this, "Internal Error", "Client manager not initialized");
        return;
    }

    if (selectedId_.is_nil()) {
        MessageBoxHelper::warning(this, "No Selection", "Please select a party.");
        return;
    }

    if (!clientManager_->selectParty(selectedId_, selectedName_)) {
        MessageBoxHelper::critical(this, "Party Selection Failed",
            "The server rejected the party selection. Please try again.");
        return;
    }

    accept();
}

}
