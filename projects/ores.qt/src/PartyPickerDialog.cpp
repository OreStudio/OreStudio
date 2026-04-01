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
#include <QHeaderView>
#include <QButtonGroup>
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
    connect(listWidget_,   &QTreeWidget::itemDoubleClicked,
            this,          [this](QTreeWidgetItem* item, int) {
        selectOperationalItem(item);
        onOkClicked();
    });

    // Refresh flag icons when images become available (warm cache or async load)
    if (imageCache_) {
        connect(imageCache_, &ImageCache::allLoaded,
                this, &PartyPickerDialog::refreshFlagIcons);
        connect(imageCache_, &ImageCache::imageLoaded,
                this, [this](const QString&) { refreshFlagIcons(); });
    }
}

boost::uuids::uuid PartyPickerDialog::selectedPartyId() const {
    return selectedId_;
}

QString PartyPickerDialog::selectedPartyName() const {
    return selectedName_;
}

// Returns a content widget with a left-indent that visually groups it under a
// radio button header.
static QWidget* makeContentWidget(QWidget* parent) {
    auto* w = new QWidget(parent);
    auto* l = new QVBoxLayout(w);
    l->setContentsMargins(20, 2, 0, 4);
    l->setSpacing(4);
    return w;
}

void PartyPickerDialog::setupUi() {
    WidgetUtils::setupComboBoxes(this);
    setWindowTitle("Select Party");
    setModal(true);
    setMinimumWidth(580);
    setFixedWidth(580);
    setSizeGripEnabled(false);

    auto* mainLayout = new QVBoxLayout(this);
    mainLayout->setSpacing(8);

    auto* infoLabel = new QLabel(
        "Your account is associated with multiple parties.\n"
        "Please select the party context for this session.", this);
    infoLabel->setWordWrap(true);
    mainLayout->addWidget(infoLabel);

    // ----------------------------------------------------------------
    // Identify system party (at most one per tenant)
    // ----------------------------------------------------------------
    const PartyInfo* systemParty = nullptr;
    for (const auto& p : parties_) {
        if (p.is_system()) { systemParty = &p; break; }
    }

    // ----------------------------------------------------------------
    // System section
    // ----------------------------------------------------------------
    systemRadio_ = new QRadioButton(
        systemParty ? systemParty->name : QString{}, this);
    systemRadio_->setVisible(systemParty != nullptr);
    mainLayout->addWidget(systemRadio_);

    // ----------------------------------------------------------------
    // Operational section
    // ----------------------------------------------------------------
    operationalRadio_ = new QRadioButton(tr("Operational Party"), this);
    mainLayout->addWidget(operationalRadio_);

    operationalContent_ = makeContentWidget(this);
    mainLayout->addWidget(operationalContent_);
    auto* opsLayout = qobject_cast<QVBoxLayout*>(operationalContent_->layout());

    // Filter row
    auto* filterRow = new QHBoxLayout();
    filterEdit_ = new QLineEdit(operationalContent_);
    filterEdit_->setPlaceholderText(tr("Filter parties..."));
    filterEdit_->setClearButtonEnabled(true);
    filterRow->addWidget(filterEdit_, 1);

    centreCombo_ = new QComboBox(operationalContent_);
    centreCombo_->setMinimumWidth(130);
    populateCentreCombo();
    filterRow->addWidget(centreCombo_);
    opsLayout->addLayout(filterRow);

    // Party tree — col 0: Centre (flag + code), col 1: Party Name
    listWidget_ = new QTreeWidget(operationalContent_);
    listWidget_->setColumnCount(2);
    listWidget_->setHeaderLabels({tr("Centre"), tr("Party Name")});
    listWidget_->setRootIsDecorated(false);
    listWidget_->setAlternatingRowColors(true);
    listWidget_->setMinimumHeight(220);
    listWidget_->setSelectionMode(QAbstractItemView::SingleSelection);
    listWidget_->header()->setStretchLastSection(true);
    listWidget_->header()->setSectionResizeMode(0, QHeaderView::ResizeToContents);
    listWidget_->header()->setSectionResizeMode(1, QHeaderView::Stretch);

    // Populate operational parties sorted alphabetically by name
    std::vector<const PartyInfo*> ops;
    for (const auto& p : parties_) {
        if (!p.is_system()) ops.push_back(&p);
    }
    std::sort(ops.begin(), ops.end(),
        [](const PartyInfo* a, const PartyInfo* b) {
            return a->name.toLower() < b->name.toLower();
        });

    for (const auto* p : ops) {
        auto* item = new QTreeWidgetItem(listWidget_);
        const QString bc = p->business_center_code;
        // col 0: business centre code (with flag icon)
        // col 1: party name
        item->setText(0, bc);
        item->setText(1, p->name);
        // UserRole on col 0: bc code; UserRole+1: party name; UserRole+2: UUID string
        item->setData(0, Qt::UserRole,     bc);
        item->setData(0, Qt::UserRole + 1, p->name);
        item->setData(0, Qt::UserRole + 2,
            QString::fromStdString(boost::uuids::to_string(p->id)));

        if (imageCache_ && !bc.isEmpty())
            item->setIcon(0, imageCache_->getBusinessCentreFlagIcon(bc.toStdString()));
    }
    opsLayout->addWidget(listWidget_);

    // ----------------------------------------------------------------
    // Mutual exclusion via QButtonGroup
    // ----------------------------------------------------------------
    auto* btnGroup = new QButtonGroup(this);
    if (systemParty) btnGroup->addButton(systemRadio_);
    btnGroup->addButton(operationalRadio_);

    auto updateSections = [this](bool systemActive) {
        operationalContent_->setEnabled(!systemActive);
        if (systemActive) {
            selectSystemParty();
        } else {
            auto* first = firstVisibleItem();
            if (first) {
                listWidget_->setCurrentItem(first);
                selectOperationalItem(first);
            }
        }
    };

    if (systemParty) {
        connect(systemRadio_, &QRadioButton::toggled, this,
            [updateSections](bool checked) { if (checked) updateSections(true); });
    }
    connect(operationalRadio_, &QRadioButton::toggled, this,
        [updateSections](bool checked) { if (checked) updateSections(false); });

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
    // Wire up filtering and list selection
    // ----------------------------------------------------------------
    connect(filterEdit_, &QLineEdit::textChanged,
            this, [this](const QString&) { applyFilter(); });
    connect(centreCombo_, QOverload<int>::of(&QComboBox::currentIndexChanged),
            this, [this](int) { applyFilter(); });

    connect(listWidget_, &QTreeWidget::itemClicked,
            this, [this](QTreeWidgetItem* item, int) {
        if (!operationalRadio_->isChecked())
            operationalRadio_->setChecked(true);
        selectOperationalItem(item);
    });

    // ----------------------------------------------------------------
    // Initial selection: system party has priority
    // ----------------------------------------------------------------
    if (systemParty) {
        systemRadio_->setChecked(true);
        // toggled signal fires synchronously, so updateSections(true) already ran
    } else {
        operationalRadio_->setChecked(true);
        auto* first = listWidget_->topLevelItem(0);
        if (first) {
            listWidget_->setCurrentItem(first);
            selectOperationalItem(first);
        }
    }

    filterEdit_->setFocus();

    // Second pass: icons for images already loaded synchronously by populateCentreCombo
    refreshFlagIcons();
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

    centreCombo_->setVisible(codes.size() > 1);
}

void PartyPickerDialog::applyFilter() {
    const QString text   = filterEdit_->text().trimmed().toLower();
    const QString centre = centreCombo_->currentData().toString();

    for (int i = 0; i < listWidget_->topLevelItemCount(); ++i) {
        auto* item = listWidget_->topLevelItem(i);
        const QString name = item->data(0, Qt::UserRole + 1).toString().toLower();
        const QString bc   = item->data(0, Qt::UserRole).toString();

        const bool nameMatch   = text.isEmpty()   || name.contains(text);
        const bool centreMatch = centre.isEmpty() || bc == centre;
        item->setHidden(!(nameMatch && centreMatch));
    }
}

QTreeWidgetItem* PartyPickerDialog::firstVisibleItem() const {
    for (int i = 0; i < listWidget_->topLevelItemCount(); ++i) {
        auto* item = listWidget_->topLevelItem(i);
        if (!item->isHidden()) return item;
    }
    return nullptr;
}

void PartyPickerDialog::selectSystemParty() {
    for (const auto& p : parties_) {
        if (!p.is_system()) continue;
        selectedId_   = p.id;
        selectedName_ = p.name;
        listWidget_->clearSelection();
        okButton_->setEnabled(true);
        return;
    }
}

void PartyPickerDialog::selectOperationalItem(QTreeWidgetItem* item) {
    if (!item) return;
    selectedName_ = item->data(0, Qt::UserRole + 1).toString();
    try {
        boost::uuids::string_generator gen;
        selectedId_ = gen(item->data(0, Qt::UserRole + 2).toString().toStdString());
    } catch (...) {
        selectedId_ = boost::uuids::uuid{};
    }
    okButton_->setEnabled(true);
}

void PartyPickerDialog::refreshFlagIcons() {
    if (!imageCache_) return;

    set_combo_flag_icons(centreCombo_, [this](const std::string& code) {
        return code.empty() ? QIcon{} : imageCache_->getBusinessCentreFlagIcon(code);
    });

    for (int i = 0; i < listWidget_->topLevelItemCount(); ++i) {
        auto* item = listWidget_->topLevelItem(i);
        const auto bc = item->data(0, Qt::UserRole).toString().toStdString();
        if (!bc.empty())
            item->setIcon(0, imageCache_->getBusinessCentreFlagIcon(bc));
    }
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
