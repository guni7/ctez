import React, { useState } from 'react';
import styled from '@emotion/styled';
import { IconButton, Grid, Box } from '@material-ui/core';
import MenuIcon from '@material-ui/icons/Menu';
import TezosIcon from '../TezosIcon';
import { Typography } from '../Typography';
import { SignIn } from '../SignIn/SignIn';
import { NavigationDrawer } from '../NavigationDrawer/NavigationDrawer';

export interface HeaderProps {
  title: string;
  onClick?: () => void | Promise<void>;
}

const HeaderActionBox = styled(Box)`
  border-bottom: 1px solid rgba(0, 0, 0, 0.1);
  padding: 15px 20px;
  display: flex;
  align-items: center;
  justify-content: space-between;

  &h1 {
    font-weight: 900;
    font-size: 20px;
    line-height: 1;
    margin: 6px 0 6px 10px;
    display: inline-block;
    vertical-align: top;
  }

  & .sign-in {
    padding-left: 64%;

    @media screen and (max-width: 768px) {
      padding-left: 51%;
    }
    @media screen and (max-width: 425px) {
      padding-left: 46%;
    }
    @media screen and (max-width: 375px) {
      padding-left: 38%;
    }
    @media screen and (max-width: 320px) {
      padding-left: 26%;
    }
  }
`;

const HeaderContainer = styled.div`
  .wrapper {
    font-family: 'Nunito Sans', 'Helvetica Neue', Helvetica, Arial, sans-serif;
    border-bottom: 1px solid rgba(0, 0, 0, 0.1);
    padding: 15px 20px;
    display: flex;
    align-items: center;
    justify-content: space-between;
  }

  svg {
    display: inline-block;
    vertical-align: top;
  }

  h1 {
    font-weight: 900;
    font-size: 20px;
    line-height: 1;
    margin: 6px 0 6px 10px;
    display: inline-block;
    vertical-align: top;
  }
`;

export const Header: React.FC<HeaderProps> = ({ title, onClick }) => {
  const [navDrawer, setNavDrawer] = useState(false);
  const handleNavDrawerClose = () => setNavDrawer(false);
  return (
    <HeaderContainer>
      <header>
        <HeaderActionBox>
          <Grid container direction="row">
            <Grid item>
              <IconButton onClick={() => setNavDrawer(true)}>
                <MenuIcon />
              </IconButton>
            </Grid>
            <Grid item>
              <div onClick={onClick} aria-hidden="true">
                <TezosIcon />
                <Typography size="body" component="h1" margin="0.4em 0 0.4em 1em">
                  {title}
                </Typography>
              </div>
            </Grid>
            <Grid item className="sign-in">
              <SignIn />
            </Grid>
          </Grid>
        </HeaderActionBox>
      </header>
      <NavigationDrawer open={navDrawer} handleDrawerClose={handleNavDrawerClose} />
    </HeaderContainer>
  );
};
